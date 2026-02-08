#ifndef R_BRIDGE_H
#define R_BRIDGE_H

#include "ring_buffer.h"
#include <stdint.h>

/* Initialize R runtime + ring buffer. Returns 0 on success. */
int rffi_init(const char *r_home);

/* Evaluate R code. Pushes output chunks to ring buffer, ending with RB_MSG_DONE.
   Returns 0 on success, -1 on error (error details in ring buffer). */
int rffi_eval(const char *code);

/* Request interruption of currently-running R evaluation. */
int rffi_interrupt(void);

/* Shutdown R and free ring buffer. */
void rffi_shutdown(void);

/* Ring buffer access (thin wrappers over global ring buffer) */
int      rffi_rb_has_data(void);
int      rffi_rb_pop(uint8_t *kind, uint8_t *flags,
                     char *out, uint32_t out_cap, uint32_t *out_len);
uint64_t rffi_rb_dropped_messages(void);
uint64_t rffi_rb_dropped_bytes(void);
void     rffi_rb_reset(void);

#endif /* R_BRIDGE_H */
