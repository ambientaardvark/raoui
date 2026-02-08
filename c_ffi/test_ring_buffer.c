#include "ring_buffer.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) printf("  %-50s ", name)
#define PASS() do { printf("OK\n"); tests_passed++; } while(0)
#define FAIL(msg) do { printf("FAIL: %s\n", msg); tests_failed++; } while(0)

#define ASSERT(cond, msg) do { \
    if (!(cond)) { FAIL(msg); return; } \
} while(0)

/* Helper: pop and check a message matches expected kind + content */
static int pop_expect(ring_buffer_t *rb, uint8_t exp_kind,
                      const char *exp_data, uint32_t exp_len) {
    uint8_t kind, flags;
    char buf[4096];
    uint32_t out_len;
    int rc = rb_pop(rb, &kind, &flags, buf, sizeof(buf), &out_len);
    if (rc != RB_OK) return -1;
    if (kind != exp_kind) return -2;
    if (out_len != exp_len) return -3;
    if (memcmp(buf, exp_data, exp_len) != 0) return -4;
    return 0;
}

/* --- Tests --- */

static void test_init_deinit(void) {
    TEST("init and deinit");
    ring_buffer_t rb;
    ASSERT(rb_init(&rb, 256) == 0, "init should succeed");
    ASSERT(rb_has_data(&rb) == 0, "should be empty after init");
    rb_deinit(&rb);
    PASS();
}

static void test_push_pop_single(void) {
    TEST("push and pop single message");
    ring_buffer_t rb;
    rb_init(&rb, 256);

    const char *msg = "hello";
    ASSERT(rb_push(&rb, RB_MSG_STDOUT, 0, msg, 5) == 0, "push failed");
    ASSERT(rb_has_data(&rb) == 1, "should have data");
    ASSERT(pop_expect(&rb, RB_MSG_STDOUT, "hello", 5) == 0, "pop mismatch");
    ASSERT(rb_has_data(&rb) == 0, "should be empty after pop");

    rb_deinit(&rb);
    PASS();
}

static void test_push_pop_multiple(void) {
    TEST("push and pop multiple messages");
    ring_buffer_t rb;
    rb_init(&rb, 512);

    rb_push(&rb, RB_MSG_STDOUT, 0, "aaa", 3);
    rb_push(&rb, RB_MSG_STDERR, 0, "bbb", 3);
    rb_push(&rb, RB_MSG_RESULT, 0, "ccc", 3);

    ASSERT(pop_expect(&rb, RB_MSG_STDOUT, "aaa", 3) == 0, "first msg");
    ASSERT(pop_expect(&rb, RB_MSG_STDERR, "bbb", 3) == 0, "second msg");
    ASSERT(pop_expect(&rb, RB_MSG_RESULT, "ccc", 3) == 0, "third msg");
    ASSERT(rb_has_data(&rb) == 0, "should be empty");

    rb_deinit(&rb);
    PASS();
}

static void test_pop_empty(void) {
    TEST("pop from empty buffer returns EMPTY");
    ring_buffer_t rb;
    rb_init(&rb, 256);

    uint8_t kind, flags;
    char buf[64];
    uint32_t out_len;
    ASSERT(rb_pop(&rb, &kind, &flags, buf, sizeof(buf), &out_len) == RB_EMPTY,
           "should return EMPTY");

    rb_deinit(&rb);
    PASS();
}

static void test_wrap_around(void) {
    TEST("wrap-around correctness");
    /* Small buffer: header is 6 bytes, so "hi" (2 bytes) = 8 bytes per frame.
       32 bytes capacity = room for 4 messages at a time. */
    ring_buffer_t rb;
    rb_init(&rb, 32);

    /* Fill and drain a few times to force wrap-around */
    for (int round = 0; round < 5; round++) {
        char msg[3];
        snprintf(msg, sizeof(msg), "%02d", round);
        rb_push(&rb, RB_MSG_STDOUT, 0, msg, 2);
        ASSERT(pop_expect(&rb, RB_MSG_STDOUT, msg, 2) == 0, "wrap-around pop");
    }

    rb_deinit(&rb);
    PASS();
}

static void test_eviction(void) {
    TEST("drop-oldest eviction");
    /* 32 bytes: fits 4 messages of 2-byte payload (8 bytes each).
       Push 5 — first should be evicted. */
    ring_buffer_t rb;
    rb_init(&rb, 32);

    rb_push(&rb, RB_MSG_STDOUT, 0, "aa", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "bb", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "cc", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "dd", 2);
    /* Buffer full. Next push should evict "aa". */
    rb_push(&rb, RB_MSG_STDOUT, 0, "ee", 2);

    ASSERT(rb_dropped_messages(&rb) == 1, "should have dropped 1");
    ASSERT(rb_dropped_bytes(&rb) == 2, "should have dropped 2 bytes");

    /* Oldest remaining should be "bb" */
    ASSERT(pop_expect(&rb, RB_MSG_STDOUT, "bb", 2) == 0, "first after evict");
    ASSERT(pop_expect(&rb, RB_MSG_STDOUT, "cc", 2) == 0, "second");
    ASSERT(pop_expect(&rb, RB_MSG_STDOUT, "dd", 2) == 0, "third");
    ASSERT(pop_expect(&rb, RB_MSG_STDOUT, "ee", 2) == 0, "fourth");
    ASSERT(rb_has_data(&rb) == 0, "should be empty");

    rb_deinit(&rb);
    PASS();
}

static void test_eviction_multiple(void) {
    TEST("eviction of multiple messages for one large push");
    /* 32 bytes capacity. Push 4 small (2-byte payload = 8 bytes each),
       then one 20-byte payload (26 bytes total frame).
       Must evict at least 3 old messages to fit. */
    ring_buffer_t rb;
    rb_init(&rb, 32);

    rb_push(&rb, RB_MSG_STDOUT, 0, "aa", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "bb", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "cc", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "dd", 2);

    char big[20];
    memset(big, 'X', 20);
    rb_push(&rb, RB_MSG_RESULT, 0, big, 20);

    ASSERT(rb_dropped_messages(&rb) >= 3, "should drop >=3 messages");

    /* Should be able to pop the big message */
    uint8_t kind, flags;
    char out[64];
    uint32_t out_len;
    int rc = rb_pop(&rb, &kind, &flags, out, sizeof(out), &out_len);
    ASSERT(rc == RB_OK, "pop big message");
    ASSERT(kind == RB_MSG_RESULT, "kind should be RESULT");
    ASSERT(out_len == 20, "payload length");

    rb_deinit(&rb);
    PASS();
}

static void test_oversize_truncation(void) {
    TEST("oversize message truncated to capacity");
    /* 32 bytes capacity. Header = 6, so max payload = 26.
       Push a 100-byte message — should be truncated. */
    ring_buffer_t rb;
    rb_init(&rb, 32);

    char big[100];
    memset(big, 'Z', 100);
    rb_push(&rb, RB_MSG_STDOUT, 0, big, 100);

    uint8_t kind, flags;
    char out[64];
    uint32_t out_len;
    int rc = rb_pop(&rb, &kind, &flags, out, sizeof(out), &out_len);
    ASSERT(rc == RB_OK, "pop should succeed");
    ASSERT(flags & RB_FLAG_TRUNCATED, "should be flagged truncated");
    ASSERT(out_len == 26, "payload should be max (capacity - header)");
    /* First 26 bytes should be 'Z' */
    for (uint32_t i = 0; i < out_len; i++) {
        ASSERT(out[i] == 'Z', "payload content");
    }

    rb_deinit(&rb);
    PASS();
}

static void test_pop_small_output_buffer(void) {
    TEST("pop with too-small output buffer");
    ring_buffer_t rb;
    rb_init(&rb, 256);

    rb_push(&rb, RB_MSG_STDOUT, 0, "hello world", 11);

    uint8_t kind, flags;
    char out[4];  /* too small for 11-byte payload */
    uint32_t out_len;
    int rc = rb_pop(&rb, &kind, &flags, out, sizeof(out), &out_len);
    ASSERT(rc == RB_TRUNCATED_COPY, "should return TRUNCATED_COPY");
    ASSERT(out_len == 4, "should copy what fits");
    ASSERT(memcmp(out, "hell", 4) == 0, "partial content");
    ASSERT(rb_has_data(&rb) == 0, "message should be consumed");

    rb_deinit(&rb);
    PASS();
}

static void test_reset(void) {
    TEST("reset clears buffer and stats");
    ring_buffer_t rb;
    rb_init(&rb, 32);

    rb_push(&rb, RB_MSG_STDOUT, 0, "aa", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "bb", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "cc", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "dd", 2);
    rb_push(&rb, RB_MSG_STDOUT, 0, "ee", 2); /* triggers eviction */

    rb_reset(&rb);
    ASSERT(rb_has_data(&rb) == 0, "should be empty after reset");
    ASSERT(rb_dropped_messages(&rb) == 0, "stats should be zeroed");
    ASSERT(rb_dropped_bytes(&rb) == 0, "stats should be zeroed");

    rb_deinit(&rb);
    PASS();
}

static void test_zero_length_message(void) {
    TEST("zero-length payload (e.g. DONE signal)");
    ring_buffer_t rb;
    rb_init(&rb, 256);

    rb_push(&rb, RB_MSG_DONE, 0, NULL, 0);
    ASSERT(rb_has_data(&rb) == 1, "should have data");

    uint8_t kind, flags;
    char out[1];
    uint32_t out_len;
    int rc = rb_pop(&rb, &kind, &flags, out, sizeof(out), &out_len);
    ASSERT(rc == RB_OK, "pop zero-length");
    ASSERT(kind == RB_MSG_DONE, "kind should be DONE");
    ASSERT(out_len == 0, "payload should be 0 bytes");

    rb_deinit(&rb);
    PASS();
}

int main(void) {
    printf("Ring buffer tests:\n");

    test_init_deinit();
    test_push_pop_single();
    test_push_pop_multiple();
    test_pop_empty();
    test_wrap_around();
    test_eviction();
    test_eviction_multiple();
    test_oversize_truncation();
    test_pop_small_output_buffer();
    test_reset();
    test_zero_length_message();

    printf("\n%d passed, %d failed\n", tests_passed, tests_failed);
    return tests_failed > 0 ? 1 : 0;
}
