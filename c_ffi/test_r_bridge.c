#include "r_bridge.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Drain and print all chunks from the ring buffer until RB_MSG_DONE.
   Returns the kind of the last non-DONE message, or -1 if only DONE. */
static int drain(void) {
    uint8_t kind, flags;
    char buf[4096];
    uint32_t len;
    int last_kind = -1;

    while (1) {
        int rc = rffi_rb_pop(&kind, &flags, buf, sizeof(buf) - 1, &len);
        if (rc == RB_EMPTY)
            continue;  /* spin until DONE arrives */
        if (rc != RB_OK) {
            printf("[drain error: rc=%d]\n", rc);
            return -1;
        }

        if (kind == RB_MSG_DONE) {
            printf("[DONE]\n");
            break;
        }

        buf[len] = '\0';
        const char *label;
        switch (kind) {
            case RB_MSG_STDOUT:  label = "STDOUT"; break;
            case RB_MSG_STDERR:  label = "STDERR"; break;
            case RB_MSG_RESULT:  label = "RESULT"; break;
            case RB_MSG_R_ERROR: label = "R_ERROR"; break;
            default:             label = "???"; break;
        }
        printf("[%s] %s", label, buf);
        if (len > 0 && buf[len - 1] != '\n')
            printf("\n");

        last_kind = kind;
    }
    return last_kind;
}

int main(void) {
    const char *r_home = getenv("R_HOME");
    if (!r_home) {
        r_home = "/Library/Frameworks/R.framework/Resources";
    }

    printf("Initializing R from %s...\n", r_home);
    if (rffi_init(r_home) != 0) {
        fprintf(stderr, "rffi_init failed\n");
        return 1;
    }
    printf("R initialized.\n\n");

    /* Test 1: simple print */
    printf("--- Test 1: simple print ---\n");
    rffi_eval("print('hello world')");
    drain();

    /* Test 2: visible result (auto-print) */
    printf("\n--- Test 2: auto-print visible result ---\n");
    rffi_eval("1 + 1");
    drain();

    /* Test 3: invisible assignment (no output) */
    printf("\n--- Test 3: invisible assignment ---\n");
    rffi_eval("x <- 42");
    drain();

    /* Test 4: multi-statement */
    printf("\n--- Test 4: multi-statement ---\n");
    rffi_eval("x <- 10\ny <- 20\nx + y");
    drain();

    /* Test 5: eval error */
    printf("\n--- Test 5: eval error ---\n");
    rffi_eval("stop('intentional error')");
    drain();

    /* Test 6: parse error */
    printf("\n--- Test 6: parse error ---\n");
    rffi_eval("1 +");
    drain();

    /* Test 7: warning */
    printf("\n--- Test 7: warning ---\n");
    rffi_eval("log(-1)");
    drain();

    /* Test 8: cat output */
    printf("\n--- Test 8: cat ---\n");
    rffi_eval("cat('line1\\nline2\\n')");
    drain();

    printf("\nAll tests completed.\n");
    rffi_shutdown();
    return 0;
}
