#include "ring_buffer.h"
#include <stdlib.h>
#include <string.h>

/* --- Internal helpers (caller must hold lock) --- */

/* Write bytes into the ring, wrapping around as needed. */
static void ring_write(ring_buffer_t *rb, const void *src, size_t n) {
    const uint8_t *s = (const uint8_t *)src;
    size_t first = rb->capacity - rb->head;
    if (first >= n) {
        memcpy(rb->buf + rb->head, s, n);
    } else {
        memcpy(rb->buf + rb->head, s, first);
        memcpy(rb->buf, s + first, n - first);
    }
    rb->head = (rb->head + n) % rb->capacity;
    rb->used += n;
}

/* Read bytes from the ring at the tail, wrapping around as needed. */
static void ring_read(ring_buffer_t *rb, void *dst, size_t n) {
    uint8_t *d = (uint8_t *)dst;
    size_t first = rb->capacity - rb->tail;
    if (first >= n) {
        memcpy(d, rb->buf + rb->tail, n);
    } else {
        memcpy(d, rb->buf + rb->tail, first);
        memcpy(d + first, rb->buf, n - first);
    }
    rb->tail = (rb->tail + n) % rb->capacity;
    rb->used -= n;
}

/* Peek at bytes from the tail without advancing. */
static void ring_peek(const ring_buffer_t *rb, void *dst, size_t n) {
    uint8_t *d = (uint8_t *)dst;
    size_t tail = rb->tail;
    size_t first = rb->capacity - tail;
    if (first >= n) {
        memcpy(d, rb->buf + tail, n);
    } else {
        memcpy(d, rb->buf + tail, first);
        memcpy(d + first, rb->buf, n - first);
    }
}

/* Drop the oldest message from the ring. Returns total frame size dropped. */
static size_t drop_oldest(ring_buffer_t *rb) {
    if (rb->used < RB_FRAME_HEADER_SIZE)
        return 0;

    rb_frame_header_t hdr;
    ring_peek(rb, &hdr, RB_FRAME_HEADER_SIZE);
    size_t frame_size = RB_FRAME_HEADER_SIZE + hdr.len;

    if (rb->used < frame_size)
        return 0;

    /* Advance tail past this frame */
    rb->tail = (rb->tail + frame_size) % rb->capacity;
    rb->used -= frame_size;

    rb->dropped_messages++;
    rb->dropped_bytes += hdr.len;

    return frame_size;
}

/* --- Public API --- */

int rb_init(ring_buffer_t *rb, size_t capacity_bytes) {
    if (!rb || capacity_bytes < RB_FRAME_HEADER_SIZE + 1)
        return -1;

    rb->buf = (uint8_t *)malloc(capacity_bytes);
    if (!rb->buf)
        return -1;

    rb->capacity = capacity_bytes;
    rb->head = 0;
    rb->tail = 0;
    rb->used = 0;
    rb->dropped_messages = 0;
    rb->dropped_bytes = 0;

    if (pthread_mutex_init(&rb->lock, NULL) != 0) {
        free(rb->buf);
        rb->buf = NULL;
        return -1;
    }

    return 0;
}

void rb_deinit(ring_buffer_t *rb) {
    if (!rb) return;
    pthread_mutex_destroy(&rb->lock);
    free(rb->buf);
    rb->buf = NULL;
    rb->capacity = 0;
    rb->head = 0;
    rb->tail = 0;
    rb->used = 0;
}

void rb_reset(ring_buffer_t *rb) {
    if (!rb) return;
    pthread_mutex_lock(&rb->lock);
    rb->head = 0;
    rb->tail = 0;
    rb->used = 0;
    rb->dropped_messages = 0;
    rb->dropped_bytes = 0;
    pthread_mutex_unlock(&rb->lock);
}

int rb_push(ring_buffer_t *rb, uint8_t kind, uint8_t flags,
            const char *data, uint32_t len) {
    if (!rb || !rb->buf) return -1;

    size_t frame_size = RB_FRAME_HEADER_SIZE + len;

    /* Message larger than total capacity: store truncated */
    if (frame_size > rb->capacity) {
        uint32_t max_payload = (uint32_t)(rb->capacity - RB_FRAME_HEADER_SIZE);
        flags |= RB_FLAG_TRUNCATED;

        pthread_mutex_lock(&rb->lock);
        /* Evict everything */
        while (rb->used > 0)
            drop_oldest(rb);
        rb_frame_header_t hdr = { .kind = kind, .flags = flags, .len = max_payload };
        ring_write(rb, &hdr, RB_FRAME_HEADER_SIZE);
        ring_write(rb, data, max_payload);
        pthread_mutex_unlock(&rb->lock);
        return 0;
    }

    pthread_mutex_lock(&rb->lock);

    /* Evict oldest messages until there's room */
    size_t free_space = rb->capacity - rb->used;
    while (free_space < frame_size) {
        size_t freed = drop_oldest(rb);
        if (freed == 0) break;  /* shouldn't happen, but guard */
        free_space = rb->capacity - rb->used;
    }

    rb_frame_header_t hdr = { .kind = kind, .flags = flags, .len = len };
    ring_write(rb, &hdr, RB_FRAME_HEADER_SIZE);
    ring_write(rb, data, len);

    pthread_mutex_unlock(&rb->lock);
    return 0;
}

int rb_has_data(ring_buffer_t *rb) {
    if (!rb) return 0;
    pthread_mutex_lock(&rb->lock);
    int has = rb->used >= RB_FRAME_HEADER_SIZE;
    pthread_mutex_unlock(&rb->lock);
    return has;
}

int rb_pop(ring_buffer_t *rb, uint8_t *kind, uint8_t *flags,
           char *out, uint32_t out_cap, uint32_t *out_len) {
    if (!rb || !rb->buf) return RB_ERROR;

    pthread_mutex_lock(&rb->lock);

    if (rb->used < RB_FRAME_HEADER_SIZE) {
        pthread_mutex_unlock(&rb->lock);
        return RB_EMPTY;
    }

    rb_frame_header_t hdr;
    ring_peek(rb, &hdr, RB_FRAME_HEADER_SIZE);

    if (rb->used < RB_FRAME_HEADER_SIZE + hdr.len) {
        /* Corrupt state — shouldn't happen */
        pthread_mutex_unlock(&rb->lock);
        return RB_ERROR;
    }

    /* Advance past header */
    rb->tail = (rb->tail + RB_FRAME_HEADER_SIZE) % rb->capacity;
    rb->used -= RB_FRAME_HEADER_SIZE;

    *kind = hdr.kind;
    *flags = hdr.flags;

    int status = RB_OK;

    if (hdr.len <= out_cap) {
        ring_read(rb, out, hdr.len);
        *out_len = hdr.len;
    } else {
        /* Caller buffer too small — copy what fits, skip the rest */
        ring_read(rb, out, out_cap);
        *out_len = out_cap;
        /* Skip remaining bytes */
        size_t remaining = hdr.len - out_cap;
        rb->tail = (rb->tail + remaining) % rb->capacity;
        rb->used -= remaining;
        status = RB_TRUNCATED_COPY;
    }

    pthread_mutex_unlock(&rb->lock);
    return status;
}

uint64_t rb_dropped_messages(ring_buffer_t *rb) {
    if (!rb) return 0;
    pthread_mutex_lock(&rb->lock);
    uint64_t val = rb->dropped_messages;
    pthread_mutex_unlock(&rb->lock);
    return val;
}

uint64_t rb_dropped_bytes(ring_buffer_t *rb) {
    if (!rb) return 0;
    pthread_mutex_lock(&rb->lock);
    uint64_t val = rb->dropped_bytes;
    pthread_mutex_unlock(&rb->lock);
    return val;
}
