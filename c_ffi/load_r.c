#include "r_bridge.h"
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/* ---- Global variable pointers (loaded via dlsym) ---- */

static SEXP *R_GlobalEnv_ptr = NULL;
static SEXP *R_NilValue_ptr  = NULL;
static int  *R_SignalHandlers_ptr = NULL;
static int  *R_interrupts_pending_ptr = NULL;

/* ---- Callback function pointers (loaded via dlsym) ---- */

static void (**ptr_R_WriteConsole)(const char *, int) = NULL;
static void (**ptr_R_WriteConsoleEx)(const char *, int, int) = NULL;
static void (**ptr_R_FlushConsole)(void) = NULL;
static void (**ptr_R_ShowMessage)(const char *) = NULL;
static FILE **R_Consolefile_ptr = NULL;
static FILE **R_Outputfile_ptr = NULL;

/* Cached R symbol for withVisible */
static SEXP withVisible_sym = NULL;

/* ---- Callbacks ---- */

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

    /* Global variables */
    LOAD_SYM(R_GlobalEnv_ptr, "R_GlobalEnv");
    LOAD_SYM(R_NilValue_ptr, "R_NilValue");
    LOAD_SYM(R_SignalHandlers_ptr, "R_SignalHandlers");
    LOAD_SYM(R_interrupts_pending_ptr, "R_interrupts_pending");

    /* Callback pointers */
    LOAD_SYM(ptr_R_WriteConsole, "ptr_R_WriteConsole");
    LOAD_SYM(ptr_R_WriteConsoleEx, "ptr_R_WriteConsoleEx");
    LOAD_SYM(ptr_R_FlushConsole, "ptr_R_FlushConsole");
    LOAD_SYM(ptr_R_ShowMessage, "ptr_R_ShowMessage");
    LOAD_SYM(R_Consolefile_ptr, "R_Consolefile");
    LOAD_SYM(R_Outputfile_ptr, "R_Outputfile");

    return 0;
}

/* ---- Public API ---- */

int rffi_init(const char *r_home) {
    if (rb_init(&g_rb, 1024 * 1024) != 0)
        return -1;

    setenv("R_HOME", r_home, 1);

    if (load_libr(r_home) != 0)
        return -1;
    if (load_symbols() != 0)
        return -1;

    *R_SignalHandlers_ptr = 0;

    char *args[] = {"raoui", "--quiet", "--no-save"};
    Rf_initialize_R(3, args);

    *ptr_R_WriteConsole = NULL;
    *ptr_R_WriteConsoleEx = cb_write_console_ex;
    *ptr_R_FlushConsole = cb_flush_console;
    *ptr_R_ShowMessage = cb_show_message;
    *R_Consolefile_ptr = NULL;
    *R_Outputfile_ptr = NULL;

    setup_Rmainloop();

    withVisible_sym = Rf_install("withVisible");

    return 0;
}

int rffi_eval(const char *code) {
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

int rffi_interrupt(void) {
    if (!R_interrupts_pending_ptr) {
        return -1;
    }
    *R_interrupts_pending_ptr = 1;
    return 0;
}

void rffi_shutdown(void) {
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
