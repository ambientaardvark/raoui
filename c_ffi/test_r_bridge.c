#include "r_bridge.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static int tests_run = 0;
static int tests_passed = 0;

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

/* Collect completions from ring buffer into caller-provided buffer.
   Returns the number of newline-separated items, or -1 on error.
   buf_out is null-terminated on success. */
static int collect_completions(char *buf_out, uint32_t buf_cap) {
    uint8_t kind, flags;
    uint32_t len;

    while (1) {
        int rc = rffi_rb_pop(&kind, &flags, buf_out, buf_cap - 1, &len);
        if (rc == RB_EMPTY) continue;
        if (rc != RB_OK) return -1;
        if (kind == RB_MSG_COMPLETIONS) {
            buf_out[len] = '\0';
            if (len == 0) return 0;
            /* Count items: number of '\n' + 1 */
            int count = 1;
            for (uint32_t i = 0; i < len; i++)
                if (buf_out[i] == '\n') count++;
            return count;
        }
    }
}

/* Check if needle appears as a complete line in the newline-separated haystack */
static int completions_contain(const char *haystack, const char *needle) {
    size_t nlen = strlen(needle);
    const char *p = haystack;
    while (p) {
        const char *nl = strchr(p, '\n');
        size_t line_len = nl ? (size_t)(nl - p) : strlen(p);
        if (line_len == nlen && strncmp(p, needle, nlen) == 0)
            return 1;
        if (!nl) break;
        p = nl + 1;
    }
    return 0;
}

#define ASSERT(cond, msg) do { \
    tests_run++; \
    if (cond) { \
        tests_passed++; \
        printf("  PASS: %s\n", msg); \
    } else { \
        printf("  FAIL: %s\n", msg); \
    } \
} while(0)

int main(void) {
    const char *r_home = getenv("R_HOME");
    if (!r_home) {
        r_home = "/Library/Frameworks/R.framework/Resources";
    }

    printf("Initializing R from %s...\n", r_home);
    if (rffi_start(r_home) != 0) {
        fprintf(stderr, "rffi_start failed\n");
        return 1;
    }
    printf("R initialized.\n\n");

    /* Test 1: simple print */
    printf("--- Test 1: simple print ---\n");
    rffi_submit("print('hello world')");
    drain();

    /* Test 2: visible result (auto-print) */
    printf("\n--- Test 2: auto-print visible result ---\n");
    rffi_submit("1 + 1");
    drain();

    /* Test 3: invisible assignment (no output) */
    printf("\n--- Test 3: invisible assignment ---\n");
    rffi_submit("x <- 42");
    drain();

    /* Test 4: multi-statement */
    printf("\n--- Test 4: multi-statement ---\n");
    rffi_submit("x <- 10\ny <- 20\nx + y");
    drain();

    /* Test 5: eval error */
    printf("\n--- Test 5: eval error ---\n");
    rffi_submit("stop('intentional error')");
    drain();

    /* Test 6: parse error */
    printf("\n--- Test 6: parse error ---\n");
    rffi_submit("1 +");
    drain();

    /* Test 7: warning */
    printf("\n--- Test 7: warning ---\n");
    rffi_submit("log(-1)");
    drain();

    /* Test 8: cat output */
    printf("\n--- Test 8: cat ---\n");
    rffi_submit("cat('line1\\nline2\\n')");
    drain();

    /* ---- Completion tests ---- */

    char buf[65536];
    int count;

    printf("\n--- Completion: \"prin\" ---\n");
    rffi_request_completions("prin", 4);
    count = collect_completions(buf, sizeof(buf));
    ASSERT(count > 0, "prin: returns completions");
    ASSERT(completions_contain(buf, "print"), "prin: contains \"print\"");
    ASSERT(completions_contain(buf, "print.default"), "prin: contains \"print.default\"");
    ASSERT(completions_contain(buf, "princomp"), "prin: contains \"princomp\"");

    printf("\n--- Completion: \"data.f\" ---\n");
    rffi_request_completions("data.f", 6);
    count = collect_completions(buf, sizeof(buf));
    ASSERT(count > 0, "data.f: returns completions");
    ASSERT(completions_contain(buf, "data.frame"), "data.f: contains \"data.frame\"");

    printf("\n--- Completion: \"xxxnonexistent\" ---\n");
    rffi_request_completions("xxxnonexistent", 14);
    count = collect_completions(buf, sizeof(buf));
    ASSERT(count == 0, "xxxnonexistent: returns empty");

    printf("\n--- Completion: empty string ---\n");
    rffi_request_completions("", 0);
    count = collect_completions(buf, sizeof(buf));
    ASSERT(count >= 0, "empty: does not crash");

    printf("\n--- Completion: \"c(1, mea\" (mid-expression) ---\n");
    rffi_request_completions("c(1, mea", 8);
    count = collect_completions(buf, sizeof(buf));
    ASSERT(count > 0, "mid-expression: returns completions");
    ASSERT(completions_contain(buf, "mean"), "mid-expression: contains \"mean\"");

    printf("\n--- Completion: string with quotes ---\n");
    rffi_request_completions("paste(\"hello\", prin", 19);
    count = collect_completions(buf, sizeof(buf));
    ASSERT(count > 0, "after string arg: returns completions");
    ASSERT(completions_contain(buf, "print"), "after string arg: contains \"print\"");

    printf("\n--- Completion: after user-defined variable ---\n");
    rffi_submit("my_test_var_xyz <- 42");
    drain();
    rffi_request_completions("my_test_var", 11);
    count = collect_completions(buf, sizeof(buf));
    ASSERT(count > 0, "user var: returns completions");
    ASSERT(completions_contain(buf, "my_test_var_xyz"), "user var: contains \"my_test_var_xyz\"");

    printf("\n========================================\n");
    printf("%d/%d tests passed\n", tests_passed, tests_run);

    rffi_shutdown();
    return tests_passed == tests_run ? 0 : 1;
}
