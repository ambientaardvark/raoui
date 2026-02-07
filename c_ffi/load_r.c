#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

static void *libR = NULL;

typedef void *SEXP;

/* Functions */
static int (*Rf_initialize_R)(int, char **) = NULL;
static void (*setup_Rmainloop)(void) = NULL;
static SEXP (*R_ParseEvalString)(const char*, SEXP) = NULL;

/* Global variables */
static SEXP *R_GlobalEnv = NULL;
static int *R_SignalHandlers = NULL;
static void (**ptr_R_WriteConsole)(const char*, int) = NULL;
static void (**ptr_R_WriteConsoleEx)(const char*, int, int) = NULL;


int load_r(const char *r_home) {
  char path[1024];
  snprintf(path, sizeof(path), "%s/lib/libR.dylib", r_home);

  libR = dlopen(path, RTLD_NOW | RTLD_GLOBAL);
  if (!libR) {
    printf("Failed to load R library: %s\n", dlerror());
    return 0;
  }
  return 1;
}

int load_symbols() {
  Rf_initialize_R = dlsym(libR, "Rf_initialize_R");
  if (!Rf_initialize_R) { printf("Failed to load Rf_initialize_R: %s\n", dlerror()); return 0; }

  setup_Rmainloop = dlsym(libR, "setup_Rmainloop");
  if (!setup_Rmainloop) { printf("Failed to load setup_Rmainloop: %s\n", dlerror()); return 0; }

  R_ParseEvalString = dlsym(libR, "R_ParseEvalString");
  if (!R_ParseEvalString) { printf("Failed to load R_ParseEvalString: %s\n", dlerror()); return 0; }

  R_GlobalEnv = dlsym(libR, "R_GlobalEnv");
  if (!R_GlobalEnv) { printf("Failed to load R_GlobalEnv: %s\n", dlerror()); return 0; }

  R_SignalHandlers = dlsym(libR, "R_SignalHandlers");
  if (!R_SignalHandlers) { printf("Failed to load R_SignalHandlers: %s\n", dlerror()); return 0; }

  ptr_R_WriteConsole = dlsym(libR, "ptr_R_WriteConsole");
  if (!ptr_R_WriteConsole) { printf("Failed to load ptr_R_WriteConsole: %s\n", dlerror()); return 0; }

  ptr_R_WriteConsoleEx = dlsym(libR, "ptr_R_WriteConsoleEx");
  if (!ptr_R_WriteConsoleEx) { printf("Failed to load ptr_R_WriteConsoleEx: %s\n", dlerror()); return 0; }

  return 1;
}

void cb_write_console_safe(const char* s, int bufline, int otype) {
    printf("\x1b[34m%s", s);
}

int main() {
    setenv("R_HOME", "/Library/Frameworks/R.framework/Resources", 1);
    load_r("/Library/Frameworks/R.framework/Resources");
    load_symbols();

    *R_SignalHandlers = 0;

    char *args[] = {"raoui", "--quiet", "--no-save"};
    Rf_initialize_R(3, args);

    *ptr_R_WriteConsole = NULL;
    *ptr_R_WriteConsoleEx = cb_write_console_safe;

    setup_Rmainloop();

    R_ParseEvalString("x <- \"World\"", *R_GlobalEnv);
    R_ParseEvalString("print(paste('Hello,', x))", *R_GlobalEnv);

    return 0;
}
