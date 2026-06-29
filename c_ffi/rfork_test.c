/* R-fork spike (throwaway): prove the run_r sandbox primitive end to end.
 *
 * Initialize an embedded R (mirroring load_r.c's dlopen/dlsym/init), define a
 * variable in the global env, then fork(). The child applies the seatbelt
 * profile, evaluates R that READS the parent's variable (proving copy-on-write
 * visibility), attempts a forbidden file write from R (proving the sandbox
 * blocks effects while R survives), then _exit()s. The parent confirms its R
 * session is untouched (variable unchanged) and still fully functional.
 *
 * Standalone, NOT via dune:
 *   R_HOME=$(R RHOME) cc -o /tmp/rfork_test c_ffi/rfork_test.c \
 *     -Wno-deprecated-declarations && /tmp/rfork_test
 * (the driver rfork_test.sh does this). */

#include <dlfcn.h>
#include <errno.h>
#include <sandbox.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#define LIBR_BASENAME "libR.dylib"
#define SCRATCH "/private/tmp/raoui-sandbox"

typedef void *SEXP;
typedef enum { PARSE_NULL, PARSE_OK, PARSE_INCOMPLETE, PARSE_ERROR, PARSE_EOF } ParseStatus;

/* ---- R API, loaded via dlsym (subset of load_r.c) ---- */
static void *libR = NULL;
static int (*Rf_initialize_R)(int, char **) = NULL;
static void (*setup_Rmainloop)(void) = NULL;
static SEXP (*R_ParseVector)(SEXP, int, ParseStatus *, SEXP) = NULL;
static SEXP (*R_tryEval)(SEXP, SEXP, int *) = NULL;
static SEXP (*Rf_mkString)(const char *) = NULL;
static SEXP (*Rf_protect)(SEXP) = NULL;
static void (*Rf_unprotect)(int) = NULL;
static SEXP (*VECTOR_ELT_fn)(SEXP, long) = NULL;
static int (*Rf_length_fn)(SEXP) = NULL;

static SEXP *R_GlobalEnv_ptr = NULL;
static SEXP *R_NilValue_ptr = NULL;
static int *R_SignalHandlers_ptr = NULL;

static int (**ptr_R_ReadConsole)(const char *, unsigned char *, int, int) = NULL;
static void (**ptr_R_WriteConsole)(const char *, int) = NULL;
static void (**ptr_R_WriteConsoleEx)(const char *, int, int) = NULL;
static void (**ptr_R_FlushConsole)(void) = NULL;
static void (**ptr_R_ShowMessage)(const char *) = NULL;
static void (**ptr_R_Suicide_ptr)(const char *) = NULL;
static FILE **R_Consolefile_ptr = NULL;
static FILE **R_Outputfile_ptr = NULL;

/* Where console output goes: -1 = real stdout (parent), else a pipe fd that
 * the child repoints after fork. Shared callback, behavior diverges via this
 * global (the child's copy is private after fork). */
static int g_capture_fd = -1;

/* ---- R console callbacks ---- */
static int cb_read_console(const char *p, unsigned char *b, int l, int h) {
  (void)p; (void)b; (void)l; (void)h;
  return 0; /* no stdin */
}
static void cb_write_console_ex(const char *s, int len, int otype) {
  (void)otype;
  if (g_capture_fd >= 0) (void)!write(g_capture_fd, s, len);
  else (void)!write(1, s, len);
}
static void cb_flush(void) {}
static void cb_show_message(const char *s) { (void)s; }
static void cb_suicide(const char *s) {
  fprintf(stderr, "R suicide: %s\n", s ? s : "");
  _exit(99);
}

#define LOAD_SYM(var, name) do { \
    *(void **)&(var) = dlsym(libR, name); \
    if (!(var)) { fprintf(stderr, "dlsym failed: %s\n", name); return -1; } \
  } while (0)

static int init_r(const char *r_home) {
  setenv("R_HOME", r_home, 1);
  char path[1024];
  snprintf(path, sizeof path, "%s/lib/%s", r_home, LIBR_BASENAME);
  libR = dlopen(path, RTLD_NOW | RTLD_GLOBAL);
  if (!libR) { fprintf(stderr, "dlopen libR: %s\n", dlerror()); return -1; }

  LOAD_SYM(Rf_initialize_R, "Rf_initialize_R");
  LOAD_SYM(setup_Rmainloop, "setup_Rmainloop");
  LOAD_SYM(R_ParseVector, "R_ParseVector");
  LOAD_SYM(R_tryEval, "R_tryEval");
  LOAD_SYM(Rf_mkString, "Rf_mkString");
  LOAD_SYM(Rf_protect, "Rf_protect");
  LOAD_SYM(Rf_unprotect, "Rf_unprotect");
  LOAD_SYM(VECTOR_ELT_fn, "VECTOR_ELT");
  LOAD_SYM(Rf_length_fn, "Rf_length");
  LOAD_SYM(R_GlobalEnv_ptr, "R_GlobalEnv");
  LOAD_SYM(R_NilValue_ptr, "R_NilValue");
  LOAD_SYM(R_SignalHandlers_ptr, "R_SignalHandlers");
  LOAD_SYM(ptr_R_ReadConsole, "ptr_R_ReadConsole");
  LOAD_SYM(ptr_R_WriteConsole, "ptr_R_WriteConsole");
  LOAD_SYM(ptr_R_WriteConsoleEx, "ptr_R_WriteConsoleEx");
  LOAD_SYM(ptr_R_FlushConsole, "ptr_R_FlushConsole");
  LOAD_SYM(ptr_R_ShowMessage, "ptr_R_ShowMessage");
  LOAD_SYM(ptr_R_Suicide_ptr, "ptr_R_Suicide");
  LOAD_SYM(R_Consolefile_ptr, "R_Consolefile");
  LOAD_SYM(R_Outputfile_ptr, "R_Outputfile");

  *R_SignalHandlers_ptr = 0;
  char *args[] = {"raoui", "--quiet", "--no-save"};
  Rf_initialize_R(3, args);
  *ptr_R_ReadConsole = cb_read_console;
  *ptr_R_WriteConsole = NULL;
  *ptr_R_WriteConsoleEx = cb_write_console_ex;
  *ptr_R_FlushConsole = cb_flush;
  *ptr_R_ShowMessage = cb_show_message;
  *ptr_R_Suicide_ptr = cb_suicide;
  *R_Consolefile_ptr = NULL;
  *R_Outputfile_ptr = NULL;
  setup_Rmainloop();
  return 0;
}

/* Parse + eval a chunk of R source in the global env. Returns 0 on success. */
static int eval_text(const char *code) {
  ParseStatus status;
  SEXP text = Rf_protect(Rf_mkString(code));
  SEXP exprs = Rf_protect(R_ParseVector(text, -1, &status, *R_NilValue_ptr));
  if (status != PARSE_OK) { Rf_unprotect(2); return -1; }
  int n = Rf_length_fn(exprs), rc = 0;
  for (int i = 0; i < n; i++) {
    int err = 0;
    R_tryEval(VECTOR_ELT_fn(exprs, i), *R_GlobalEnv_ptr, &err);
    if (err) rc = -1;
  }
  Rf_unprotect(2);
  return rc;
}

/* Same seatbelt profile as the standalone spike: allow-default, deny writes
 * (except a scratch dir + /dev/null), network, and exec. */
static const char *PROFILE =
    "(version 1)\n"
    "(allow default)\n"
    "(deny file-write*)\n"
    "(allow file-write* (subpath \"" SCRATCH "\") (literal \"/dev/null\"))\n"
    "(deny network*)\n"
    "(deny process-exec*)\n";

static void enter_sandbox(void) {
  char *err = NULL;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  if (sandbox_init(PROFILE, 0, &err) != 0) {
#pragma clang diagnostic pop
    fprintf(stderr, "sandbox_init failed: %s\n", err ? err : "?");
    _exit(98);
  }
}

int main(void) {
  /* Unbuffered so our printf narration and R's raw-write console output appear
     in true chronological order rather than the printfs flushing at exit. */
  setvbuf(stdout, NULL, _IONBF, 0);
  const char *r_home = getenv("R_HOME");
  if (!r_home) { fprintf(stderr, "set R_HOME (e.g. $(R RHOME))\n"); return 2; }
  if (init_r(r_home) != 0) return 1;

  /* Parent: establish session state the child should see but not alter. */
  eval_text("x <- 42L");
  printf("parent: defined x <- 42L, about to fork\n");
  fflush(stdout);

  int pipefd[2];
  if (pipe(pipefd) != 0) { perror("pipe"); return 1; }

  pid_t pid = fork();
  if (pid < 0) { perror("fork"); return 1; }

  if (pid == 0) {
    /* ---- Child: sandboxed R evaluation ---- */
    close(pipefd[0]);
    enter_sandbox();
    g_capture_fd = pipefd[1]; /* route R console output to the pipe */

    char home_path[1024];
    const char *home = getenv("HOME");
    snprintf(home_path, sizeof home_path, "%s/rfork_leak.txt", home ? home : "/tmp");

    char code[2048];
    snprintf(code, sizeof code,
             "cat('child: sees x =', x, '\\n')\n"
             "cat('child: x*2 =', x * 2L, '\\n')\n"
             "x <- 999L\n"
             "cat('child: mutated local x to', x, '\\n')\n"
             "res <- tryCatch({ writeLines('leak', '%s'); 'WROTE (BAD)' },\n"
             "               error = function(e) paste('blocked:', conditionMessage(e)))\n"
             "cat('child: file write ->', res, '\\n')\n",
             home_path);
    eval_text(code);
    close(pipefd[1]);
    _exit(0);
  }

  /* ---- Parent: capture child output, then verify integrity ---- */
  close(pipefd[1]);
  char buf[4096];
  ssize_t n;
  printf("---- captured from sandboxed child ----\n");
  while ((n = read(pipefd[0], buf, sizeof buf)) > 0) (void)!write(1, buf, n);
  close(pipefd[0]);
  int wstatus = 0;
  waitpid(pid, &wstatus, 0);
  printf("---- child exited (status=%d) ----\n",
         WIFEXITED(wstatus) ? WEXITSTATUS(wstatus) : -1);

  printf("parent: checking session is intact and still works\n");
  eval_text("cat('parent: x is still', x, '(expect 42)\\n')");
  eval_text("x <- x + 1L; cat('parent: x+1 =', x, '(expect 43 — R still works)\\n')");

  const char *home = getenv("HOME");
  char leak[1024];
  snprintf(leak, sizeof leak, "%s/rfork_leak.txt", home ? home : "/tmp");
  if (access(leak, F_OK) == 0)
    printf("parent: LEAK FILE EXISTS at %s — sandbox FAILED\n", leak);
  else
    printf("parent: no leak file — sandbox blocked the write\n");

  return 0;
}
