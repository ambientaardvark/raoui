#include "r_bridge.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

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

/* Extract token (first line) from completion response, return items start.
   Returns pointer to the first completion item (after first \n), or NULL. */
static const char *parse_completion_response(const char *buf, char *token_out, size_t token_cap) {
    const char *nl = strchr(buf, '\n');
    if (!nl) {
        size_t len = strlen(buf);
        if (len < token_cap) { memcpy(token_out, buf, len); token_out[len] = '\0'; }
        else { token_out[0] = '\0'; }
        return NULL;
    }
    size_t tlen = (size_t)(nl - buf);
    if (tlen < token_cap) { memcpy(token_out, buf, tlen); token_out[tlen] = '\0'; }
    else { token_out[0] = '\0'; }
    return nl + 1;
}

/* Collect completions from ring buffer.
   Parses response: first line = token, rest = completion items.
   buf_out receives just the items (newline-separated), token goes to token_out.
   Returns the number of items, or -1 on error. */
static int collect_completions(char *buf_out, uint32_t buf_cap,
                               char *token_out, size_t token_cap) {
    uint8_t kind, flags;
    char raw[65536];
    uint32_t len;

    while (1) {
        int rc = rffi_rb_pop(&kind, &flags, raw, sizeof(raw) - 1, &len);
        if (rc == RB_EMPTY) continue;
        if (rc != RB_OK) return -1;
        if (kind == RB_MSG_COMPLETIONS) {
            raw[len] = '\0';
            if (len == 0) {
                token_out[0] = '\0';
                buf_out[0] = '\0';
                return 0;
            }
            const char *items = parse_completion_response(raw, token_out, token_cap);
            if (!items || *items == '\0') {
                buf_out[0] = '\0';
                return 0;
            }
            size_t ilen = strlen(items);
            if (ilen < buf_cap) { memcpy(buf_out, items, ilen); buf_out[ilen] = '\0'; }
            else { buf_out[0] = '\0'; return 0; }
            int count = 1;
            for (size_t i = 0; i < ilen; i++)
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
    char token[256];
    int count;

    printf("\n--- Completion: \"prin\" ---\n");
    rffi_request_completions("prin", 4);
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count > 0, "prin: returns completions");
    ASSERT(strcmp(token, "prin") == 0, "prin: token is \"prin\"");
    ASSERT(completions_contain(buf, "print"), "prin: contains \"print\"");
    ASSERT(completions_contain(buf, "print.default"), "prin: contains \"print.default\"");
    ASSERT(completions_contain(buf, "princomp"), "prin: contains \"princomp\"");

    printf("\n--- Completion: \"data.f\" ---\n");
    rffi_request_completions("data.f", 6);
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count > 0, "data.f: returns completions");
    ASSERT(strcmp(token, "data.f") == 0, "data.f: token is \"data.f\"");
    ASSERT(completions_contain(buf, "data.frame"), "data.f: contains \"data.frame\"");

    printf("\n--- Completion: \"xxxnonexistent\" ---\n");
    rffi_request_completions("xxxnonexistent", 14);
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count == 0, "xxxnonexistent: returns empty");

    printf("\n--- Completion: empty string ---\n");
    rffi_request_completions("", 0);
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count >= 0, "empty: does not crash");

    printf("\n--- Completion: \"c(1, mea\" (mid-expression) ---\n");
    rffi_request_completions("c(1, mea", 8);
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count > 0, "mid-expression: returns completions");
    ASSERT(strcmp(token, "mea") == 0, "mid-expression: token is \"mea\"");
    ASSERT(completions_contain(buf, "mean"), "mid-expression: contains \"mean\"");

    printf("\n--- Completion: string with quotes ---\n");
    rffi_request_completions("paste(\"hello\", prin", 19);
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count > 0, "after string arg: returns completions");
    ASSERT(completions_contain(buf, "print"), "after string arg: contains \"print\"");

    printf("\n--- Completion: after user-defined variable ---\n");
    rffi_submit("my_test_var_xyz <- 42");
    drain();
    rffi_request_completions("my_test_var", 11);
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count > 0, "user var: returns completions");
    ASSERT(completions_contain(buf, "my_test_var_xyz"), "user var: contains \"my_test_var_xyz\"");

    printf("\n--- Column completion: data.frame ---\n");
    rffi_submit("my_test_df <- data.frame(alpha = 1, beta_value = 2)");
    drain();
    rffi_request_columns("my_test_df");
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count == 2, "data.frame columns: returns two columns");
    ASSERT(strcmp(token, "") == 0, "data.frame columns: token is empty");
    ASSERT(completions_contain(buf, "alpha"), "data.frame columns: contains \"alpha\"");
    ASSERT(completions_contain(buf, "beta_value"), "data.frame columns: contains \"beta_value\"");

    printf("\n--- Column completion: tibble-like data frame ---\n");
    rffi_submit("my_test_tbl <- data.frame(gamma = 1, delta_value = 2); class(my_test_tbl) <- c('tbl_df', 'tbl', 'data.frame')");
    drain();
    rffi_request_columns("my_test_tbl");
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count == 2, "tibble columns: returns two columns");
    ASSERT(completions_contain(buf, "gamma"), "tibble columns: contains \"gamma\"");
    ASSERT(completions_contain(buf, "delta_value"), "tibble columns: contains \"delta_value\"");

    printf("\n--- Column completion: non-data object ---\n");
    rffi_submit("my_test_scalar <- 42");
    drain();
    rffi_request_columns("my_test_scalar");
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count == 0, "non-data object columns: returns empty");

    printf("\n--- Column completion: missing object ---\n");
    rffi_request_columns("my_missing_df");
    count = collect_completions(buf, sizeof(buf), token, sizeof(token));
    ASSERT(count == 0, "missing object columns: returns empty");

    /* ---- Readline test ---- */

    printf("\n--- Readline: simple prompt ---\n");
    rffi_submit("name <- readline('Enter name: ')");

    /* Wait for RB_MSG_READLINE */
    uint8_t kind, flags;
    char rbuf[4096];
    uint32_t len;
    int found_readline = 0;
    int attempts = 0;

    while (!found_readline && attempts < 1000) {
        int rc = rffi_rb_pop(&kind, &flags, rbuf, sizeof(rbuf) - 1, &len);
        if (rc == RB_EMPTY) {
            usleep(1000);
            attempts++;
            continue;
        }
        if (rc == RB_OK && kind == RB_MSG_READLINE) {
            rbuf[len] = '\0';
            printf("[READLINE] prompt: '%s'\n", rbuf);
            ASSERT(strcmp(rbuf, "Enter name: ") == 0, "readline: correct prompt");
            found_readline = 1;

            /* Submit input */
            rffi_submit_readline_input("Alice");
            printf("Submitted input: 'Alice'\n");
        }
    }

    if (!found_readline) {
        printf("  FAIL: readline message not received after 1000 attempts\n");
        tests_run++;
    }

    /* Drain remaining messages */
    drain();

    printf("\n========================================\n");
    printf("%d/%d tests passed\n", tests_passed, tests_run);

    rffi_shutdown();
    return tests_passed == tests_run ? 0 : 1;
}
