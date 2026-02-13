#ifndef R_BRIDGE_H
#define R_BRIDGE_H

#include "ring_buffer.h"
#include <stdint.h>

/* Start the R worker thread. Blocks until R is initialized.
   Returns 0 on success. The thread runs a loop that processes
   submitted commands and R events between evaluations. */
int rffi_start(const char *r_home);

/* Submit code to the R worker thread for evaluation.
   Non-blocking: just posts to the command queue. */
void rffi_submit(const char *code);

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

/* Signal the passthrough gate so the R thread can proceed with system(). */
void rffi_signal_passthrough(void);

#endif /* R_BRIDGE_H */
