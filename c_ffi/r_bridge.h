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

/* Run R in a sandboxed fork; returns malloc'd captured output (caller frees). */
char *rffi_run_r_sandboxed(const char *code);

/* Take the console output captured during R init (.Rprofile, .First, site
   profile), transferring ownership to the caller (frees it). Returns NULL if
   nothing was captured. Call once, after rffi_start returns. */
char *rffi_take_init_output(void);

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

/* Request tab-completions for the given line and cursor position.
   Non-blocking: posts to the worker thread's queue.
   Result arrives via RB_MSG_COMPLETIONS in the ring buffer. */
void rffi_request_completions(const char *line, int cursor_pos);

/* Request column names for a data-frame-like R object by name.
   Non-blocking: posts to the worker thread's queue.
   Result arrives via RB_MSG_COMPLETIONS with an empty token line followed by
   column names, one per line. Non-data-frame objects return no items. */
void rffi_request_columns(const char *object_name);

/* Signal the passthrough gate so the R thread can proceed with system(). */
void rffi_signal_passthrough(void);

/* Configure the path used by fatal crash handlers. */
void rffi_set_crash_log_path(const char *path);

/* Submit input for readline() callback and signal the readline gate. */
void rffi_submit_readline_input(const char *input);

#endif /* R_BRIDGE_H */
