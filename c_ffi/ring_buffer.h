#ifndef RING_BUFFER_H
#define RING_BUFFER_H

#include <stdint.h>
#include <stddef.h>
#include <pthread.h>

/* Message kinds */
#define RB_MSG_STDOUT         0
#define RB_MSG_STDERR         1
#define RB_MSG_RESULT         2
#define RB_MSG_R_ERROR        3
#define RB_MSG_INTERNAL_ERROR 4
#define RB_MSG_DONE           5
#define RB_MSG_IMAGE          6
#define RB_MSG_NOTICE         7

/* Message flags */
#define RB_FLAG_NONE      0
#define RB_FLAG_TRUNCATED 1

/* Return codes for rb_pop */
#define RB_OK             0
#define RB_EMPTY          1
#define RB_TRUNCATED_COPY 2
#define RB_ERROR          3

/* Frame header stored in the ring buffer (packed) */
typedef struct __attribute__((packed)) {
    uint8_t  kind;
    uint8_t  flags;
    uint32_t len;
} rb_frame_header_t;

#define RB_FRAME_HEADER_SIZE sizeof(rb_frame_header_t)

/* Ring buffer state */
typedef struct {
    uint8_t *buf;
    size_t   capacity;
    size_t   head;        /* write position (next byte to write) */
    size_t   tail;        /* read position (next byte to read) */
    size_t   used;        /* bytes currently occupied */
    uint64_t dropped_messages;
    uint64_t dropped_bytes;
    pthread_mutex_t lock;
} ring_buffer_t;

/* Lifecycle */
int  rb_init(ring_buffer_t *rb, size_t capacity_bytes);
void rb_deinit(ring_buffer_t *rb);
void rb_reset(ring_buffer_t *rb);

/* Producer (R callback side) — acquires lock internally */
int rb_push(ring_buffer_t *rb, uint8_t kind, uint8_t flags,
            const char *data, uint32_t len);

/* Consumer (OCaml side) — acquires lock internally */
int rb_has_data(ring_buffer_t *rb);
int rb_pop(ring_buffer_t *rb, uint8_t *kind, uint8_t *flags,
           char *out, uint32_t out_cap, uint32_t *out_len);

/* Stats */
uint64_t rb_dropped_messages(ring_buffer_t *rb);
uint64_t rb_dropped_bytes(ring_buffer_t *rb);

#endif /* RING_BUFFER_H */
