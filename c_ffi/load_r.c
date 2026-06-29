#include "r_bridge.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <poll.h>
#include <time.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sandbox.h>

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
static char *crash_log_path = NULL;

/* Command queue */
static pthread_mutex_t cmd_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cmd_cond = PTHREAD_COND_INITIALIZER;
static char *pending_cmd = NULL;
static char *pending_completion_line = NULL;
static int   pending_completion_pos  = 0;
static char *pending_column_object = NULL;
static atomic_int shutdown_flag = 0;

/* Sandboxed run_r request/response. Its own channel, separate from the REPL
   ring buffer: the AI's sandboxed eval output goes back to the caller as a
   string, never onto the user's screen. pending_runr/runr_result are guarded
   by cmd_mutex; runr_call_mutex serializes concurrent callers (claude issues
   tool calls one at a time, but be safe). */
static char *pending_runr = NULL;   /* code awaiting sandboxed eval on worker */
static char *runr_result = NULL;    /* malloc'd captured output for the caller */
static int   runr_done = 0;         /* response-ready flag */
static pthread_cond_t  runr_done_cond  = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t runr_call_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Child-only: fd that R console output is routed to during sandboxed eval. */
static int g_runr_pipe_fd = -1;

/* ---- Crash logging ---- */

static void append_literal(int fd, const char *s) {
    size_t len = strlen(s);
    while (len > 0) {
        ssize_t written = write(fd, s, len);
        if (written <= 0)
            return;
        s += written;
        len -= (size_t)written;
    }
}

static void append_int(int fd, int value) {
    char buf[32];
    int len = 0;
    unsigned int n;

    if (value < 0) {
        buf[len++] = '-';
        n = (unsigned int)(-value);
    } else {
        n = (unsigned int)value;
    }

    if (n == 0) {
        buf[len++] = '0';
    } else {
        char digits[16];
        int digits_len = 0;
        while (n > 0 && digits_len < (int)sizeof(digits)) {
            digits[digits_len++] = (char)('0' + (n % 10));
            n /= 10;
        }
        while (digits_len > 0)
            buf[len++] = digits[--digits_len];
    }

    (void)write(fd, buf, (size_t)len);
}

static void crash_signal_handler(int signum) {
    const char *path = crash_log_path ? crash_log_path : "raoui-crash.log";
    int fd = open(path, O_WRONLY | O_CREAT | O_APPEND, 0600);

    if (fd >= 0) {
        append_literal(fd, "fatal signal ");
        append_int(fd, signum);
        append_literal(fd, " in rffi bridge\n");
        close(fd);
    }

    signal(signum, SIG_DFL);
    raise(signum);
}

static void install_crash_signal_handlers(void) {
    static int installed = 0;
    if (installed)
        return;

    int signals[] = { SIGABRT, SIGSEGV, SIGBUS, SIGILL, SIGFPE };
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = crash_signal_handler;
    sigemptyset(&sa.sa_mask);

    for (size_t i = 0; i < sizeof(signals) / sizeof(signals[0]); i++)
        sigaction(signals[i], &sa, NULL);

    installed = 1;
}

void rffi_set_crash_log_path(const char *path) {
    free(crash_log_path);
    crash_log_path = path ? strdup(path) : NULL;
    install_crash_signal_handlers();
}

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
static SEXP (*Rf_lang1)(SEXP) = NULL;
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
static void (**ptr_R_Suicide_ptr)(const char *) = NULL;
static FILE **R_Consolefile_ptr = NULL;
static FILE **R_Outputfile_ptr = NULL;

/* ---- R_Suicide handler ---- */

static void cb_suicide(const char *s) {
    fprintf(stderr, "\nFatal R error: %s\n", s);
    _exit(1);
}

/* DLL registration (for .Call from R) */
typedef struct { const char *name; void *fun; int numArgs; } R_CallMethodDef_t;
static void *(*R_getEmbeddingDllInfo_fn)(void) = NULL;
static int  (*R_registerRoutines_fn)(void *, const void *,
              const R_CallMethodDef_t *, const void *, const void *) = NULL;

/* Cached R symbol for withVisible */
static SEXP withVisible_sym = NULL;
static SEXP raoui_after_top_level_sym = NULL;
static SEXP (*Rf_findVar)(SEXP, SEXP) = NULL;
static SEXP *R_UnboundValue_ptr = NULL;

/* ---- Toplevel execution wrappers ---- */

/* Runs fn(data) inside R_ToplevelExec if available, otherwise directly.
   Returns 1 on success, 0 if R longjmp'd out. */
static int toplevel_exec(void (*fn)(void *), void *data) {
    if (R_ToplevelExec_fn)
        return R_ToplevelExec_fn(fn, data);
    fn(data);
    return 1;
}

typedef struct {
    SEXP text;
    int num;
    ParseStatus *status;
    SEXP src;
    SEXP result;
} parse_data_t;

static void safe_parse(void *data) {
    parse_data_t *d = (parse_data_t *)data;
    d->result = R_ParseVector(d->text, d->num, d->status, d->src);
}

typedef struct {
    SEXP expr;
    SEXP env;
    int *error;
    SEXP result;
} eval_data_t;

static void safe_eval(void *data) {
    eval_data_t *d = (eval_data_t *)data;
    d->result = R_tryEval(d->expr, d->env, d->error);
}

/* Autoprint a value under R_ToplevelExec. An S3 print method can itself error
   (e.g. print.ggplot opens a device then its stat aborts); without a live
   top-level context here that error longjmps to the dangling jump buffer left
   by setup_Rmainloop and crashes the process. */
static void safe_print(void *data) {
    Rf_PrintValue(*(SEXP *)data);
}

static void run_after_top_level_hook(void) {
    /* Only call after the startup script has defined the function */
    SEXP fn = Rf_findVar(raoui_after_top_level_sym, *R_GlobalEnv_ptr);
    if (fn == *R_UnboundValue_ptr) return;

    int hook_error = 0;
    SEXP hook_call = Rf_protect(Rf_lang1(raoui_after_top_level_sym));
    eval_data_t hook_eval = { hook_call, *R_GlobalEnv_ptr, &hook_error, NULL };
    (void)toplevel_exec(safe_eval, &hook_eval);
    Rf_unprotect(1);
}

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

static SEXP raoui_emit_image(SEXP source_path, SEXP preview_path, SEXP width, SEXP height, SEXP mime) {
    const char *source_path_str = R_CHAR_fn(Rf_asChar(source_path));
    const char *preview_path_str = R_CHAR_fn(Rf_asChar(preview_path));
    const char *width_str = R_CHAR_fn(Rf_asChar(width));
    const char *height_str = R_CHAR_fn(Rf_asChar(height));
    const char *mime_str = R_CHAR_fn(Rf_asChar(mime));
    if (!source_path_str || source_path_str[0] == '\0' ||
        !preview_path_str || preview_path_str[0] == '\0') {
        return *R_NilValue_ptr;
    }
    char payload[8192];
    int written = snprintf(payload, sizeof(payload),
        "source_path=%s\npreview_path=%s\nmime=%s\nwidth=%s\nheight=%s\n",
        source_path_str, preview_path_str, mime_str, width_str, height_str);
    if (written < 0) {
        return *R_NilValue_ptr;
    }
    if ((size_t)written >= sizeof(payload)) {
        return *R_NilValue_ptr;
    }
    rb_push(&g_rb, RB_MSG_IMAGE, 0, payload, (uint32_t)written);
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
    LOAD_SYM(Rf_lang1, "Rf_lang1");
    LOAD_SYM(Rf_lang2, "Rf_lang2");
    LOAD_SYM(Rf_asLogical, "Rf_asLogical");

    /* String conversion */
    LOAD_SYM(Rf_asChar, "Rf_asChar");
    LOAD_SYM(R_CHAR_fn, "R_CHAR");

    /* Variable binding / lookup */
    LOAD_SYM(Rf_defineVar, "Rf_defineVar");
    LOAD_SYM(Rf_findVar, "Rf_findVar");

    /* Global variables */
    LOAD_SYM(R_GlobalEnv_ptr, "R_GlobalEnv");
    LOAD_SYM(R_NilValue_ptr, "R_NilValue");
    LOAD_SYM(R_UnboundValue_ptr, "R_UnboundValue");
    LOAD_SYM(R_SignalHandlers_ptr, "R_SignalHandlers");
    LOAD_SYM(R_interrupts_pending_ptr, "R_interrupts_pending");

    /* Callback pointers */
    LOAD_SYM(ptr_R_ReadConsole, "ptr_R_ReadConsole");
    LOAD_SYM(ptr_R_WriteConsole, "ptr_R_WriteConsole");
    LOAD_SYM(ptr_R_WriteConsoleEx, "ptr_R_WriteConsoleEx");
    LOAD_SYM(ptr_R_FlushConsole, "ptr_R_FlushConsole");
    LOAD_SYM(ptr_R_ShowMessage, "ptr_R_ShowMessage");
    LOAD_SYM(ptr_R_Suicide_ptr, "ptr_R_Suicide");
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
    *ptr_R_Suicide_ptr = cb_suicide;
    *R_Consolefile_ptr = NULL;
    *R_Outputfile_ptr = NULL;

    setup_Rmainloop();

    withVisible_sym = Rf_install("withVisible");
    raoui_after_top_level_sym = Rf_install("raoui_after_top_level");

    /* Register .Call-able passthrough functions */
    void *dll = R_getEmbeddingDllInfo_fn();
    R_CallMethodDef_t callMethods[] = {
        {"raoui_enter_passthrough",
         (void *)raoui_enter_passthrough, 0},
        {"raoui_exit_passthrough",
         (void *)raoui_exit_passthrough, 0},
        {"raoui_emit_image",
         (void *)raoui_emit_image, 5},
        {NULL, NULL, 0}
    };
    R_registerRoutines_fn(dll, NULL, callMethods, NULL, NULL);

    return 0;
}

/* ---- Eval (called on the R worker thread) ---- */

static int eval_code(const char *code) {
    *R_interrupts_pending_ptr = 0;

    SEXP code_sexp = Rf_protect(Rf_mkString(code));

    ParseStatus parse_status = PARSE_ERROR;
    parse_data_t pd = { code_sexp, -1, &parse_status, *R_NilValue_ptr, NULL };
    toplevel_exec(safe_parse, &pd);
    SEXP parsed = Rf_protect(pd.result ? pd.result : *R_NilValue_ptr);

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

        eval_data_t ed = { wv_call, *R_GlobalEnv_ptr, &error_occurred, NULL };
        if (!toplevel_exec(safe_eval, &ed))
            error_occurred = 1;
        SEXP wv_result = Rf_protect(ed.result ? ed.result : *R_NilValue_ptr);

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
            if (!toplevel_exec(safe_print, &value)) {
                error_occurred = 1;
                Rf_unprotect(3);
                rb_push(&g_rb, RB_MSG_R_ERROR, 0, "", 0);
                break;
            }
            Rf_unprotect(1);
        }

        run_after_top_level_hook();

        Rf_unprotect(2);
    }

    rb_push(&g_rb, RB_MSG_DONE, 0, NULL, 0);
    Rf_unprotect(2);
    return error_occurred ? -1 : 0;
}

/* ---- Eval-for-string (returns C string, no output side effects) ---- */

static char *eval_for_string(const char *code) {
    SEXP code_sexp = Rf_protect(Rf_mkString(code));

    ParseStatus ps = PARSE_ERROR;
    parse_data_t pd = { code_sexp, -1, &ps, *R_NilValue_ptr, NULL };
    toplevel_exec(safe_parse, &pd);
    SEXP parsed = Rf_protect(pd.result ? pd.result : *R_NilValue_ptr);

    if (ps != PARSE_OK) {
        Rf_unprotect(2);
        return NULL;
    }

    int n = Rf_length_fn(parsed);
    SEXP result = *R_NilValue_ptr;
    int err = 0;

    for (int i = 0; i < n; i++) {
        eval_data_t ed = { VECTOR_ELT_fn(parsed, i), *R_GlobalEnv_ptr, &err, NULL };
        if (!toplevel_exec(safe_eval, &ed))
            err = 1;
        result = ed.result;
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

/* ---- Sandboxed run_r (fork + seatbelt; runs on the R worker thread) ---- */

/* Wall-clock watchdog: kill a child that *blocks* (deadlock, hung I/O) rather
   than burning CPU — RLIMIT_CPU only catches busy loops, and a wedged child
   would otherwise freeze the R worker thread. Set above the 10s CPU limit so a
   CPU-bound loop dies on RLIMIT_CPU first and gets the more specific message. */
#define RUNR_WALL_TIMEOUT_MS 30000
/* R vector-heap ceiling: macOS rejects RLIMIT_AS, so cap allocations inside R
   instead — an oversized alloc fails with a clean R error, not OS thrash. */
#define RUNR_MAX_VSIZE_MB "4096"

/* Console callback used only inside the forked child: route R's output to the
   capture pipe instead of the ring buffer. The override is COW-private to the
   child, so the parent's callback is untouched. */
static void cb_runr_capture(const char *s, int len, int otype) {
    (void)otype;
    if (g_runr_pipe_fd >= 0)
        (void)!write(g_runr_pipe_fd, s, (size_t)len);
}

/* Growable string buffer used to assemble the SBPL profile. */
typedef struct {
    char *buf;   /* heap buffer, always NUL-terminated */
    size_t len;  /* bytes used, excluding the NUL */
    size_t cap;  /* allocated capacity */
} sbuf;

/* Ensure room for [extra] more bytes plus the NUL. */
static void sb_reserve(sbuf *s, size_t extra) {
    if (s->len + extra + 1 > s->cap) {
        s->cap = (s->len + extra + 1) * 2;
        s->buf = realloc(s->buf, s->cap);
    }
}

static void sb_puts(sbuf *s, const char *str) {
    size_t l = strlen(str);
    sb_reserve(s, l);
    memcpy(s->buf + s->len, str, l);
    s->len += l;
    s->buf[s->len] = '\0';
}

/* Append a `  (subpath "<path>")` rule, escaping " and \ for SBPL. */
static void sb_put_subpath(sbuf *s, const char *path) {
    sb_puts(s, "  (subpath \"");
    sb_reserve(s, 2 * strlen(path));
    for (const char *p = path; *p; p++) {
        if (*p == '"' || *p == '\\') s->buf[s->len++] = '\\';
        s->buf[s->len++] = *p;
    }
    s->buf[s->len] = '\0';
    sb_puts(s, "\")\n");
}

/* Build the seatbelt profile for one run_r call (malloc'd; caller frees).
   Runs pre-fork on the worker thread with R live, so the read allow-list can be
   pinned to R's own machinery + the working directory rather than hardcoded.

   Posture (SBPL is last-match-wins, mirroring the existing write rule):
     - allow default, then deny the effects AI code must not have:
       file writes (bar a scratch dir + /dev/null), network, exec;
     - deny file reads by default, then re-allow R's machinery and the cwd, so
       the model can't read (and thus exfiltrate via its output) secrets like
       ~/.ssh or ~/.aws that live elsewhere under $HOME; finally a trailing deny
       re-closes well-known credential stores under $HOME even if an allowed
       path (e.g. cwd == $HOME) would otherwise cover them.

   The child forks an already-initialized R, so the base session is in memory
   copy-on-write — these read paths only need to cover what AI code triggers
   *after* the fork: lazy library() loads (.libPaths()), locale/timezone data
   (/usr, /private/var), and data files (cwd). Anything else is a clean EPERM. */
static char *build_runr_profile(void) {
    /* Ask the live R where its machinery lives (newline-joined). */
    char *dyn = eval_for_string(
        "paste(unique(c(R.home(), .libPaths(), tempdir(), getwd())), "
        "collapse=\"\\n\")");

    /* System roots R/dyld touch lazily after the fork. */
    static const char *roots[] = {
        "/usr", "/System", "/Library", "/opt/homebrew",
        "/private/var", "/etc", "/private/etc", "/dev",
    };

    sbuf s = { malloc(4096), 0, 4096 };
    s.buf[0] = '\0';
    sb_puts(&s, "(version 1)\n(allow default)\n");
    /* writes: deny all but the scratch dir and /dev/null */
    sb_puts(&s,
        "(deny file-write*)\n"
        "(allow file-write* (subpath \"/private/tmp/raoui-sandbox\") "
        "(literal \"/dev/null\"))\n");
    sb_puts(&s, "(deny network*)\n(deny process-exec*)\n");
    /* reads: deny by default, then re-allow R's machinery + cwd */
    sb_puts(&s, "(deny file-read*)\n(allow file-read*\n");
    for (size_t i = 0; i < sizeof roots / sizeof roots[0]; i++)
        sb_put_subpath(&s, roots[i]);
    if (dyn) {
        char *save = NULL;
        for (char *tok = strtok_r(dyn, "\n", &save); tok;
             tok = strtok_r(NULL, "\n", &save))
            if (*tok) sb_put_subpath(&s, tok);
    }
    sb_puts(&s, ")\n");
    /* Belt-and-suspenders secret deny: a path allowed above can still cover a
       credential store — most importantly when raoui is launched from $HOME, so
       getwd() == $HOME re-allows the whole home tree, but also a .libPaths()
       entry sitting next to secrets. Trailing deny wins under last-match-wins,
       so re-close the well-known credential locations under $HOME regardless. */
    const char *home = getenv("HOME");
    if (home && *home) {
        static const char *secrets[] = {
            "/.ssh", "/.aws", "/.gnupg", "/.config", "/.netrc",
            "/.Renviron", "/.Rhistory", "/.bash_history", "/.zsh_history",
            "/.docker", "/.kube", "/Library/Keychains",
        };
        sb_puts(&s, "(deny file-read*\n");
        for (size_t i = 0; i < sizeof secrets / sizeof secrets[0]; i++) {
            char path[4096];
            snprintf(path, sizeof path, "%s%s", home, secrets[i]);
            sb_put_subpath(&s, path);
        }
        sb_puts(&s, ")\n");
    }
    free(dyn);
    return s.buf;
}

/* Parse + eval R in the child, autoprinting visible values, just like the REPL
   but with output already routed to the capture pipe. R_tryEval prints any
   error text through the (repointed) console callback, so errors land in the
   captured output too; we just stop the loop. */
static void eval_in_sandbox(const char *code) {
    *R_interrupts_pending_ptr = 0;
    SEXP code_sexp = Rf_protect(Rf_mkString(code));
    ParseStatus ps = PARSE_ERROR;
    parse_data_t pd = { code_sexp, -1, &ps, *R_NilValue_ptr, NULL };
    toplevel_exec(safe_parse, &pd);
    SEXP parsed = Rf_protect(pd.result ? pd.result : *R_NilValue_ptr);
    if (ps != PARSE_OK) {
        const char *msg = "Parse error\n";
        if (g_runr_pipe_fd >= 0) (void)!write(g_runr_pipe_fd, msg, strlen(msg));
        Rf_unprotect(2);
        return;
    }
    int n = Rf_length_fn(parsed);
    for (int i = 0; i < n; i++) {
        SEXP wv_call = Rf_protect(Rf_lang2(withVisible_sym, VECTOR_ELT_fn(parsed, i)));
        int err = 0;
        eval_data_t ed = { wv_call, *R_GlobalEnv_ptr, &err, NULL };
        if (!toplevel_exec(safe_eval, &ed)) err = 1;
        SEXP wv_result = Rf_protect(ed.result ? ed.result : *R_NilValue_ptr);
        if (err) { Rf_unprotect(2); break; }  /* error text already on the pipe */
        SEXP value   = VECTOR_ELT_fn(wv_result, 0);
        SEXP visible = VECTOR_ELT_fn(wv_result, 1);
        if (Rf_asLogical(visible)) {
            Rf_protect(value);
            if (!toplevel_exec(safe_print, &value)) { Rf_unprotect(3); break; }
            Rf_unprotect(1);
        }
        Rf_unprotect(2);
    }
    Rf_unprotect(2);
}

/* Fork the worker, sandbox + eval in the child, capture its output in the
   parent, and return it malloc'd (caller frees). See issue #32: forking this
   multithreaded process carries a low-probability allocator-lock hazard. */
static char *run_r_on_worker(const char *code) {
    /* Built pre-fork while R is live so the read allow-list tracks R's paths. */
    char *profile = build_runr_profile();

    int pipefd[2];
    if (pipe(pipefd) != 0) { free(profile); return strdup("[run_r] pipe() failed\n"); }

    pid_t pid = fork();
    if (pid < 0) {
        close(pipefd[0]); close(pipefd[1]);
        free(profile);
        return strdup("[run_r] fork() failed\n");
    }

    if (pid == 0) {
        /* ---- Child: only the worker thread survives the fork ---- */
        close(pipefd[0]);
        /* Runaway guard: a CPU-bound infinite loop dies with SIGXCPU. */
        struct rlimit rl = { 10, 10 };
        setrlimit(RLIMIT_CPU, &rl);
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
        char *sberr = NULL;
        if (sandbox_init(profile, 0, &sberr) != 0) {
            const char *m = "[run_r] sandbox_init failed\n";
            (void)!write(pipefd[1], m, strlen(m));
            _exit(98);
        }
#pragma clang diagnostic pop
        g_runr_pipe_fd = pipefd[1];
        *ptr_R_WriteConsoleEx = cb_runr_capture;
        *ptr_R_WriteConsole = NULL;
        /* Cap R's vector heap (guarded for R < 4.2 where mem.maxVSize is absent);
           an oversized allocation then fails with a clean R error. */
        free(eval_for_string(
            "if (exists('mem.maxVSize')) invisible(mem.maxVSize(" RUNR_MAX_VSIZE_MB "))"));
        eval_in_sandbox(code);
        close(pipefd[1]);
        _exit(0);
    }

    /* ---- Parent (worker thread): drain with a wall-clock watchdog, reap ---- */
    close(pipefd[1]);
    size_t cap = 8192, len = 0;
    char *buf = malloc(cap);
    int timed_out = 0;
    struct timespec t0;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    for (;;) {
        struct timespec tn;
        clock_gettime(CLOCK_MONOTONIC, &tn);
        long elapsed_ms = (tn.tv_sec - t0.tv_sec) * 1000
                        + (tn.tv_nsec - t0.tv_nsec) / 1000000;
        long remaining = RUNR_WALL_TIMEOUT_MS - elapsed_ms;
        if (remaining <= 0) { timed_out = 1; break; }
        struct pollfd pfd = { pipefd[0], POLLIN, 0 };
        int pr = poll(&pfd, 1, (int)remaining);
        if (pr == 0) { timed_out = 1; break; }       /* watchdog fired */
        if (pr < 0) { if (errno == EINTR) continue; break; }
        char tmp[4096];
        ssize_t n = read(pipefd[0], tmp, sizeof tmp);
        if (n > 0) {
            if (len + (size_t)n + 1 > cap) {
                cap = (len + (size_t)n + 1) * 2;
                buf = realloc(buf, cap);
            }
            memcpy(buf + len, tmp, (size_t)n);
            len += (size_t)n;
        } else if (n == 0) {
            break;                                    /* child closed pipe (EOF) */
        } else if (errno != EINTR) {
            break;
        }
    }
    close(pipefd[0]);

    /* Watchdog fired: the child is wedged on something RLIMIT_CPU can't catch
       (deadlock, blocked I/O, the #32 fork hazard) — kill it so the worker
       thread doesn't hang. */
    if (timed_out) kill(pid, SIGKILL);

    free(profile);  /* parent's copy; the child has its own COW copy */

    int st = 0;
    waitpid(pid, &st, 0);

    /* Annotate abnormal termination so the AI understands truncated output. */
    char note[128];
    int m = 0;
    if (timed_out)
        m = snprintf(note, sizeof note, "\n[run_r: timed out after %ds — killed]\n",
                     RUNR_WALL_TIMEOUT_MS / 1000);
    else if (WIFSIGNALED(st)) {
        int sig = WTERMSIG(st);
        m = snprintf(note, sizeof note, "\n[run_r: child killed by signal %d%s]\n",
                     sig, sig == SIGXCPU ? " — CPU limit" : "");
    }
    if (m > 0) {
        if (len + (size_t)m + 1 > cap) { cap = len + (size_t)m + 1; buf = realloc(buf, cap); }
        memcpy(buf + len, note, (size_t)m);
        len += (size_t)m;
    }
    buf[len] = '\0';
    return buf;
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

static void run_column_completions(const char *object_name) {
    /* Set .raoui_column_object in R's global env (avoids string escaping) */
    SEXP sym = Rf_install(".raoui_column_object");
    SEXP val = Rf_protect(Rf_mkString(object_name));
    Rf_defineVar(sym, val, *R_GlobalEnv_ptr);
    Rf_unprotect(1);

    const char *code =
        "local({"
        "  object_name <- .raoui_column_object;"
        "  cols <- tryCatch({"
        "    obj <- get(object_name, envir=.GlobalEnv, inherits=TRUE);"
        "    if (is.data.frame(obj)) names(obj) else character();"
        "  }, error=function(e) character());"
        "  paste0(c('', cols), collapse='\\n')"
        "})";

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
        if (!pending_cmd && !pending_completion_line && !pending_column_object
            && !pending_runr && !atomic_load(&shutdown_flag)) {
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
            free(pending_column_object);
            pending_column_object = NULL;
            pthread_mutex_unlock(&cmd_mutex);
            rb_reset(&g_rb);
            eval_code(code);
            free(code);
        } else if (pending_runr) {
            char *code = pending_runr;
            pending_runr = NULL;
            pthread_mutex_unlock(&cmd_mutex);
            char *out = run_r_on_worker(code);  /* fork + sandbox + capture */
            free(code);
            pthread_mutex_lock(&cmd_mutex);
            runr_result = out;
            runr_done = 1;
            pthread_cond_signal(&runr_done_cond);
            pthread_mutex_unlock(&cmd_mutex);
        } else if (pending_completion_line) {
            char *line = pending_completion_line;
            int pos = pending_completion_pos;
            pending_completion_line = NULL;
            free(pending_column_object);
            pending_column_object = NULL;
            pthread_mutex_unlock(&cmd_mutex);
            run_completions(line, pos);
            free(line);
        } else if (pending_column_object) {
            char *object_name = pending_column_object;
            pending_column_object = NULL;
            pthread_mutex_unlock(&cmd_mutex);
            run_column_completions(object_name);
            free(object_name);
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

    /* Run R on a large stack. macOS gives secondary threads only 512 KB by
       default, far too little for R's deep recursion (e.g. rlang/cli error
       backtraces), which overflows and crashes the process. */
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, 64 * 1024 * 1024);
    pthread_create(&r_thread, &attr, r_thread_func, strdup(r_home));
    pthread_attr_destroy(&attr);

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

/* Synchronously run AI-supplied R in a sandboxed fork and return its captured
   output (caller frees). Blocks until the worker thread has forked, evaluated,
   and reaped the child — call from a systhread so the UI loop stays live. */
char *rffi_run_r_sandboxed(const char *code) {
    pthread_mutex_lock(&runr_call_mutex);   /* one sandboxed eval at a time */
    pthread_mutex_lock(&cmd_mutex);
    free(pending_runr);
    pending_runr = strdup(code);
    runr_done = 0;
    runr_result = NULL;
    pthread_cond_signal(&cmd_cond);
    while (!runr_done)
        pthread_cond_wait(&runr_done_cond, &cmd_mutex);
    char *res = runr_result;
    runr_result = NULL;
    pthread_mutex_unlock(&cmd_mutex);
    pthread_mutex_unlock(&runr_call_mutex);
    return res;
}

void rffi_request_completions(const char *line, int cursor_pos) {
    pthread_mutex_lock(&cmd_mutex);
    free(pending_completion_line);
    pending_completion_line = strdup(line);
    pending_completion_pos = cursor_pos;
    free(pending_column_object);
    pending_column_object = NULL;
    pthread_cond_signal(&cmd_cond);
    pthread_mutex_unlock(&cmd_mutex);
}

void rffi_request_columns(const char *object_name) {
    pthread_mutex_lock(&cmd_mutex);
    free(pending_column_object);
    pending_column_object = strdup(object_name);
    free(pending_completion_line);
    pending_completion_line = NULL;
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
