#include "r_bridge.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include <unistd.h>
#include <sys/time.h>

#if defined(__APPLE__)
#define LIBR_BASENAME "libR.dylib"
#else
#define LIBR_BASENAME "libR.so"
#endif

/* ---- R types ---- */

typedef void *SEXP;

typedef enum {
    PARSE_NULL,
    PARSE_OK,
    PARSE_INCOMPLETE,
    PARSE_ERROR,
    PARSE_EOF
} ParseStatus;

/* ---- Global state ---- */

static ring_buffer_t g_rb;
static void *libR = NULL;
static atomic_int passthrough_gate = 0;
static atomic_int readline_gate = 0;
static char *readline_input_buf = NULL;
static pthread_mutex_t readline_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Worker thread */
static pthread_t r_thread;
static pthread_mutex_t init_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t init_cond = PTHREAD_COND_INITIALIZER;
static int init_done = 0;
static int init_result = -1;

/* Command queue */
static pthread_mutex_t cmd_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cmd_cond = PTHREAD_COND_INITIALIZER;
static char *pending_cmd = NULL;
static char *pending_completion_line = NULL;
static int   pending_completion_pos  = 0;
static atomic_int shutdown_flag = 0;

/* ---- Function pointers (loaded via dlsym) ---- */

/* Lifecycle */
static int  (*Rf_initialize_R)(int, char **) = NULL;
static void (*setup_Rmainloop)(void) = NULL;

/* Parsing + eval */
static SEXP (*R_ParseVector)(SEXP, int, ParseStatus *, SEXP) = NULL;
static SEXP (*R_tryEval)(SEXP, SEXP, int *) = NULL;
static SEXP (*Rf_mkString)(const char *) = NULL;

/* GC protection */
static SEXP (*Rf_protect)(SEXP) = NULL;
static void (*Rf_unprotect)(int) = NULL;

/* Vector access */
static SEXP (*VECTOR_ELT_fn)(SEXP, int) = NULL;
static int  (*Rf_length_fn)(SEXP) = NULL;

/* Output */
static void (*Rf_PrintValue)(SEXP) = NULL;

/* Symbol/call building */
static SEXP (*Rf_install)(const char *) = NULL;
static SEXP (*Rf_lang2)(SEXP, SEXP) = NULL;
static int  (*Rf_asLogical)(SEXP) = NULL;

/* String conversion */
static SEXP        (*Rf_asChar)(SEXP) = NULL;
static const char *(*R_CHAR_fn)(SEXP) = NULL;

/* Variable binding */
static void (*Rf_defineVar)(SEXP, SEXP, SEXP) = NULL;

/* Event processing (optional — may be NULL on some platforms) */
static int  (*R_ToplevelExec_fn)(void (*)(void *), void *) = NULL;
static void (*R_ProcessEvents_fn)(void) = NULL;
static void *(*R_checkActivity_fn)(int, int) = NULL;
static void (*R_runHandlers_fn)(void *, void *) = NULL;
static void **R_InputHandlers_ptr = NULL;

/* ---- Global variable pointers (loaded via dlsym) ---- */

static SEXP *R_GlobalEnv_ptr = NULL;
static SEXP *R_NilValue_ptr  = NULL;
static int  *R_SignalHandlers_ptr = NULL;
static int  *R_interrupts_pending_ptr = NULL;

/* ---- Callback function pointers (loaded via dlsym) ---- */

static int  (**ptr_R_ReadConsole)(const char *, unsigned char *, int, int) = NULL;
static void (**ptr_R_WriteConsole)(const char *, int) = NULL;
static void (**ptr_R_WriteConsoleEx)(const char *, int, int) = NULL;
static void (**ptr_R_FlushConsole)(void) = NULL;
static void (**ptr_R_ShowMessage)(const char *) = NULL;
static FILE **R_Consolefile_ptr = NULL;
static FILE **R_Outputfile_ptr = NULL;

/* DLL registration (for .Call from R) */
typedef struct { const char *name; void *fun; int numArgs; } R_CallMethodDef_t;
static void *(*R_getEmbeddingDllInfo_fn)(void) = NULL;
static int  (*R_registerRoutines_fn)(void *, const void *,
              const R_CallMethodDef_t *, const void *, const void *) = NULL;

/* Cached R symbol for withVisible */
static SEXP withVisible_sym = NULL;

/* ---- Callbacks ---- */

static int cb_read_console(const char *prompt, unsigned char *buf, int len,
                           int addtohistory) {
    (void)addtohistory;

    rb_push(&g_rb, RB_MSG_READLINE, 0, prompt, (uint32_t)strlen(prompt));

    while (!atomic_load(&readline_gate))
        usleep(1000);
    atomic_store(&readline_gate, 0);

    pthread_mutex_lock(&readline_mutex);
    const char *input = readline_input_buf ? readline_input_buf : "";
    size_t n = strlen(input);
    /* R expects a newline-terminated string; reserve room for '\n' + '\0' */
    if ((int)(n + 2) > len) n = (size_t)(len - 2);
    memcpy(buf, input, n);
    buf[n]     = '\n';
    buf[n + 1] = '\0';
    free(readline_input_buf);
    readline_input_buf = NULL;
    pthread_mutex_unlock(&readline_mutex);

    return 1; /* 1 = success, 0 = EOF */
}

static void cb_write_console_ex(const char *s, int len, int otype) {
    uint8_t kind = (otype == 0) ? RB_MSG_STDOUT : RB_MSG_STDERR;
    rb_push(&g_rb, kind, 0, s, (uint32_t)len);
}

static void cb_flush_console(void) {
    /* No-op for now. The OCaml polling loop drains frequently.
       Could push a flush signal here later for tighter progress bar timing. */
}

static void cb_show_message(const char *s) {
    rb_push(&g_rb, RB_MSG_STDERR, 0, s, (uint32_t)strlen(s));
}

/* ---- Passthrough for system() ---- */

static SEXP raoui_enter_passthrough(void) {
    rb_push(&g_rb, RB_MSG_PASSTHROUGH, 0, NULL, 0);
    while (!atomic_load(&passthrough_gate))
        usleep(1000);
    atomic_store(&passthrough_gate, 0);
    return *R_NilValue_ptr;
}

static SEXP raoui_exit_passthrough(void) {
    rb_push(&g_rb, RB_MSG_PASSTHROUGH_END, 0, NULL, 0);
    return *R_NilValue_ptr;
}

void rffi_signal_passthrough(void) {
    atomic_store(&passthrough_gate, 1);
}

void rffi_submit_readline_input(const char *input) {
    pthread_mutex_lock(&readline_mutex);
    free(readline_input_buf);
    readline_input_buf = strdup(input);
    pthread_mutex_unlock(&readline_mutex);
    atomic_store(&readline_gate, 1);
}

/* ---- Symbol loading ---- */

#define LOAD_SYM(var, name) do { \
    *(void **)&(var) = dlsym(libR, name); \
    if (!(var)) { \
        fprintf(stderr, "dlsym failed: %s: %s\n", name, dlerror()); \
        return -1; \
    } \
} while(0)

static int load_libr(const char *r_home) {
    char path[1024];
    snprintf(path, sizeof(path), "%s/lib/%s", r_home, LIBR_BASENAME);

    libR = dlopen(path, RTLD_NOW | RTLD_GLOBAL);
    if (!libR) {
        fprintf(stderr, "Failed to load libR: %s\n", dlerror());
        return -1;
    }
    return 0;
}

static int load_symbols(void) {
    /* Lifecycle */
    LOAD_SYM(Rf_initialize_R, "Rf_initialize_R");
    LOAD_SYM(setup_Rmainloop, "setup_Rmainloop");

    /* Parsing + eval */
    LOAD_SYM(R_ParseVector, "R_ParseVector");
    LOAD_SYM(R_tryEval, "R_tryEval");
    LOAD_SYM(Rf_mkString, "Rf_mkString");

    /* GC protection */
    LOAD_SYM(Rf_protect, "Rf_protect");
    LOAD_SYM(Rf_unprotect, "Rf_unprotect");

    /* Vector access */
    LOAD_SYM(VECTOR_ELT_fn, "VECTOR_ELT");
    LOAD_SYM(Rf_length_fn, "Rf_length");

    /* Output */
    LOAD_SYM(Rf_PrintValue, "Rf_PrintValue");

    /* Symbol/call building */
    LOAD_SYM(Rf_install, "Rf_install");
    LOAD_SYM(Rf_lang2, "Rf_lang2");
    LOAD_SYM(Rf_asLogical, "Rf_asLogical");

    /* String conversion */
    LOAD_SYM(Rf_asChar, "Rf_asChar");
    LOAD_SYM(R_CHAR_fn, "R_CHAR");

    /* Variable binding */
    LOAD_SYM(Rf_defineVar, "Rf_defineVar");

    /* Global variables */
    LOAD_SYM(R_GlobalEnv_ptr, "R_GlobalEnv");
    LOAD_SYM(R_NilValue_ptr, "R_NilValue");
    LOAD_SYM(R_SignalHandlers_ptr, "R_SignalHandlers");
    LOAD_SYM(R_interrupts_pending_ptr, "R_interrupts_pending");

    /* Callback pointers */
    LOAD_SYM(ptr_R_ReadConsole, "ptr_R_ReadConsole");
    LOAD_SYM(ptr_R_WriteConsole, "ptr_R_WriteConsole");
    LOAD_SYM(ptr_R_WriteConsoleEx, "ptr_R_WriteConsoleEx");
    LOAD_SYM(ptr_R_FlushConsole, "ptr_R_FlushConsole");
    LOAD_SYM(ptr_R_ShowMessage, "ptr_R_ShowMessage");
    LOAD_SYM(R_Consolefile_ptr, "R_Consolefile");
    LOAD_SYM(R_Outputfile_ptr, "R_Outputfile");

    /* DLL registration */
    LOAD_SYM(R_getEmbeddingDllInfo_fn, "R_getEmbeddingDllInfo");
    LOAD_SYM(R_registerRoutines_fn, "R_registerRoutines");

    /* Event processing (optional — not fatal if missing) */
    *(void **)&R_ToplevelExec_fn   = dlsym(libR, "R_ToplevelExec");
    *(void **)&R_ProcessEvents_fn  = dlsym(libR, "R_ProcessEvents");
    *(void **)&R_checkActivity_fn  = dlsym(libR, "R_checkActivity");
    *(void **)&R_runHandlers_fn    = dlsym(libR, "R_runHandlers");
    *(void **)&R_InputHandlers_ptr = dlsym(libR, "R_InputHandlers");

    return 0;
}

/* ---- Event processing (keeps httpgd, later, etc. alive) ---- */

static void process_events_inner(void *data) {
    (void)data;
    if (R_ProcessEvents_fn)
        R_ProcessEvents_fn();
    if (R_checkActivity_fn && R_runHandlers_fn && R_InputHandlers_ptr) {
        void *what = R_checkActivity_fn(0, 1);
        if (what) R_runHandlers_fn(*R_InputHandlers_ptr, what);
    }
}

static void process_events(void) {
    if (R_ToplevelExec_fn)
        R_ToplevelExec_fn(process_events_inner, NULL);
}

/* ---- R initialization (called on the R worker thread) ---- */

static int init_r(const char *r_home) {
    setenv("R_HOME", r_home, 1);

    if (load_libr(r_home) != 0)
        return -1;
    if (load_symbols() != 0)
        return -1;

    *R_SignalHandlers_ptr = 0;

    char *args[] = {"raoui", "--quiet", "--no-save"};
    Rf_initialize_R(3, args);

    *ptr_R_ReadConsole = cb_read_console;
    *ptr_R_WriteConsole = NULL;
    *ptr_R_WriteConsoleEx = cb_write_console_ex;
    *ptr_R_FlushConsole = cb_flush_console;
    *ptr_R_ShowMessage = cb_show_message;
    *R_Consolefile_ptr = NULL;
    *R_Outputfile_ptr = NULL;

    setup_Rmainloop();

    withVisible_sym = Rf_install("withVisible");

    /* Register .Call-able passthrough functions */
    void *dll = R_getEmbeddingDllInfo_fn();
    R_CallMethodDef_t callMethods[] = {
        {"raoui_enter_passthrough",
         (void *)raoui_enter_passthrough, 0},
        {"raoui_exit_passthrough",
         (void *)raoui_exit_passthrough, 0},
        {NULL, NULL, 0}
    };
    R_registerRoutines_fn(dll, NULL, callMethods, NULL, NULL);

    return 0;
}

/* ---- Eval (called on the R worker thread) ---- */

static int eval_code(const char *code) {
    *R_interrupts_pending_ptr = 0;

    SEXP code_sexp = Rf_protect(Rf_mkString(code));

    ParseStatus parse_status;
    SEXP parsed = Rf_protect(R_ParseVector(
        code_sexp, -1, &parse_status, *R_NilValue_ptr));

    if (parse_status != PARSE_OK) {
        const char *msg;
        switch (parse_status) {
            case PARSE_INCOMPLETE:
                msg = "Parse error: incomplete expression";
                break;
            case PARSE_ERROR:
                msg = "Parse error";
                break;
            default:
                msg = "Parse error: unknown";
                break;
        }
        rb_push(&g_rb, RB_MSG_R_ERROR, 0, msg, (uint32_t)strlen(msg));
        rb_push(&g_rb, RB_MSG_DONE, 0, NULL, 0);
        Rf_unprotect(2);
        return -1;
    }

    int n = Rf_length_fn(parsed);
    int error_occurred = 0;

    for (int i = 0; i < n; i++) {
        SEXP expr = VECTOR_ELT_fn(parsed, i);

        /* Wrap in withVisible(expr) to get value + visibility */
        SEXP wv_call = Rf_protect(Rf_lang2(withVisible_sym, expr));
        SEXP wv_result = Rf_protect(
            R_tryEval(wv_call, *R_GlobalEnv_ptr, &error_occurred));

        if (error_occurred) {
            Rf_unprotect(2);
            rb_push(&g_rb, RB_MSG_R_ERROR, 0, "", 0);
            break;
        }

        /* withVisible returns list(value=, visible=) */
        SEXP value   = VECTOR_ELT_fn(wv_result, 0);
        SEXP visible = VECTOR_ELT_fn(wv_result, 1);

        if (Rf_asLogical(visible)) {
            Rf_protect(value);
            Rf_PrintValue(value);
            Rf_unprotect(1);
        }

        Rf_unprotect(2);
    }

    rb_push(&g_rb, RB_MSG_DONE, 0, NULL, 0);
    Rf_unprotect(2);
    return error_occurred ? -1 : 0;
}

/* ---- Eval-for-string (returns C string, no output side effects) ---- */

static char *eval_for_string(const char *code) {
    SEXP code_sexp = Rf_protect(Rf_mkString(code));

    ParseStatus ps;
    SEXP parsed = Rf_protect(R_ParseVector(
        code_sexp, -1, &ps, *R_NilValue_ptr));

    if (ps != PARSE_OK) {
        Rf_unprotect(2);
        return NULL;
    }

    int n = Rf_length_fn(parsed);
    SEXP result = *R_NilValue_ptr;
    int err = 0;

    for (int i = 0; i < n; i++) {
        result = R_tryEval(VECTOR_ELT_fn(parsed, i),
                           *R_GlobalEnv_ptr, &err);
        if (err) {
            Rf_unprotect(2);
            return NULL;
        }
    }

    Rf_protect(result);
    SEXP char_sexp = Rf_protect(Rf_asChar(result));
    const char *str = R_CHAR_fn(char_sexp);
    char *out = strdup(str);

    Rf_unprotect(4);
    return out;
}

/* ---- Completions ---- */

static void run_completions(const char *line, int cursor_pos) {
    /* Set .raoui_compl in R's global env (avoids string escaping) */
    SEXP sym = Rf_install(".raoui_compl");
    SEXP val = Rf_protect(Rf_mkString(line));
    Rf_defineVar(sym, val, *R_GlobalEnv_ptr);
    Rf_unprotect(1);

    char code[512];
    snprintf(code, sizeof(code),
        "local({"
        "  utils:::.assignLinebuffer(.raoui_compl);"
        "  utils:::.assignEnd(%dL);"
        "  utils:::.guessTokenFromLine();"
        "  utils:::.completeToken();"
        "  token <- get('token', envir=utils:::.CompletionEnv);"
        "  comps <- utils:::.retrieveCompletions();"
        "  paste0(c(token, comps), collapse=\"\\n\")"
        "})", cursor_pos);

    char *result = eval_for_string(code);
    if (result) {
        rb_push(&g_rb, RB_MSG_COMPLETIONS, 0, result, (uint32_t)strlen(result));
        free(result);
    } else {
        rb_push(&g_rb, RB_MSG_COMPLETIONS, 0, "", 0);
    }
}

/* ---- Worker thread ---- */

static void *r_thread_func(void *arg) {
    char *r_home = (char *)arg;

    int rc = init_r(r_home);
    free(r_home);

    /* Signal init complete */
    pthread_mutex_lock(&init_mutex);
    init_result = rc;
    init_done = 1;
    pthread_cond_signal(&init_cond);
    pthread_mutex_unlock(&init_mutex);

    if (rc != 0) return NULL;

    /* Worker loop: wait for commands, process R events while idle */
    while (!atomic_load(&shutdown_flag)) {
        pthread_mutex_lock(&cmd_mutex);
        if (!pending_cmd && !pending_completion_line
            && !atomic_load(&shutdown_flag)) {
            struct timeval tv;
            gettimeofday(&tv, NULL);
            struct timespec ts;
            ts.tv_sec = tv.tv_sec;
            ts.tv_nsec = tv.tv_usec * 1000 + 100000000; /* +100ms */
            if (ts.tv_nsec >= 1000000000) {
                ts.tv_sec++;
                ts.tv_nsec -= 1000000000;
            }
            pthread_cond_timedwait(&cmd_cond, &cmd_mutex, &ts);
        }

        if (pending_cmd) {
            char *code = pending_cmd;
            pending_cmd = NULL;
            /* Discard stale completion request */
            free(pending_completion_line);
            pending_completion_line = NULL;
            pthread_mutex_unlock(&cmd_mutex);
            rb_reset(&g_rb);
            eval_code(code);
            free(code);
        } else if (pending_completion_line) {
            char *line = pending_completion_line;
            int pos = pending_completion_pos;
            pending_completion_line = NULL;
            pthread_mutex_unlock(&cmd_mutex);
            run_completions(line, pos);
            free(line);
        } else {
            pthread_mutex_unlock(&cmd_mutex);
        }

        process_events();
    }

    return NULL;
}

/* ---- Public API ---- */

int rffi_start(const char *r_home) {
    if (rb_init(&g_rb, 1024 * 1024) != 0)
        return -1;

    pthread_create(&r_thread, NULL, r_thread_func, strdup(r_home));

    /* Block until R initialization completes on the worker thread */
    pthread_mutex_lock(&init_mutex);
    while (!init_done)
        pthread_cond_wait(&init_cond, &init_mutex);
    int rc = init_result;
    pthread_mutex_unlock(&init_mutex);

    return rc;
}

void rffi_submit(const char *code) {
    pthread_mutex_lock(&cmd_mutex);
    free(pending_cmd);
    pending_cmd = strdup(code);
    pthread_cond_signal(&cmd_cond);
    pthread_mutex_unlock(&cmd_mutex);
}

void rffi_request_completions(const char *line, int cursor_pos) {
    pthread_mutex_lock(&cmd_mutex);
    free(pending_completion_line);
    pending_completion_line = strdup(line);
    pending_completion_pos = cursor_pos;
    pthread_cond_signal(&cmd_cond);
    pthread_mutex_unlock(&cmd_mutex);
}

int rffi_interrupt(void) {
    if (!R_interrupts_pending_ptr) {
        return -1;
    }
    *R_interrupts_pending_ptr = 1;
    return 0;
}

void rffi_shutdown(void) {
    atomic_store(&shutdown_flag, 1);
    pthread_cond_signal(&cmd_cond);
    pthread_join(r_thread, NULL);
    rb_deinit(&g_rb);
    if (libR) {
        dlclose(libR);
        libR = NULL;
    }
}

/* ---- Ring buffer accessors ---- */

int rffi_rb_has_data(void) {
    return rb_has_data(&g_rb);
}

int rffi_rb_pop(uint8_t *kind, uint8_t *flags,
                char *out, uint32_t out_cap, uint32_t *out_len) {
    return rb_pop(&g_rb, kind, flags, out, out_cap, out_len);
}

uint64_t rffi_rb_dropped_messages(void) {
    return rb_dropped_messages(&g_rb);
}

uint64_t rffi_rb_dropped_bytes(void) {
    return rb_dropped_bytes(&g_rb);
}

void rffi_rb_reset(void) {
    rb_reset(&g_rb);
}
