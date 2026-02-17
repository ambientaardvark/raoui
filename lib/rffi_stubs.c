#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/threads.h>
#include <string.h>
#include <stdlib.h>
#include "r_bridge.h"
#include "ring_buffer.h"

#define CAML_RFFI_POP_BUF_CAP (1024u * 1024u)

/* Releases the OCaml runtime lock while blocking on R initialization,
   so the Eio scheduler can keep running. */
CAMLprim value caml_rffi_init(value v_r_home) {
    char *home = strdup(String_val(v_r_home));
    caml_release_runtime_system();
    int rc = rffi_start(home);
    caml_acquire_runtime_system();
    free(home);
    return Val_int(rc);
}

/* Non-blocking: just posts to the R worker thread's command queue. */
CAMLprim value caml_rffi_submit(value v_code) {
    rffi_submit(String_val(v_code));
    return Val_unit;
}

CAMLprim value caml_rffi_shutdown(value v_unit) {
    (void)v_unit;
    rffi_shutdown();
    return Val_unit;
}

CAMLprim value caml_rffi_interrupt(value v_unit) {
    (void)v_unit;
    return Val_int(rffi_interrupt());
}

CAMLprim value caml_rffi_rb_has_data(value v_unit) {
    (void)v_unit;
    return Val_bool(rffi_rb_has_data());
}

/* Returns (int * int * string) option:
   None if buffer empty, Some (kind, flags, payload) otherwise. */
CAMLprim value caml_rffi_rb_pop(value v_unit) {
    CAMLparam1(v_unit);
    CAMLlocal3(v_some, v_tuple, v_payload);

    uint8_t kind, flags;
    char *buf = malloc(CAML_RFFI_POP_BUF_CAP);
    uint32_t out_len;
    if (!buf) {
        CAMLreturn(Val_int(0)); /* None */
    }

    int rc = rffi_rb_pop(&kind, &flags, buf, CAML_RFFI_POP_BUF_CAP, &out_len);
    if (rc == RB_EMPTY || rc == RB_ERROR) {
        free(buf);
        CAMLreturn(Val_int(0)); /* None */
    }

    v_payload = caml_alloc_string(out_len);
    memcpy(Bytes_val(v_payload), buf, out_len);
    free(buf);

    v_tuple = caml_alloc_tuple(3);
    Store_field(v_tuple, 0, Val_int(kind));
    Store_field(v_tuple, 1, Val_int(flags));
    Store_field(v_tuple, 2, v_payload);

    v_some = caml_alloc(1, 0);
    Store_field(v_some, 0, v_tuple);

    CAMLreturn(v_some);
}

CAMLprim value caml_rffi_rb_reset(value v_unit) {
    (void)v_unit;
    rffi_rb_reset();
    return Val_unit;
}

CAMLprim value caml_rffi_signal_passthrough(value v_unit) {
    (void)v_unit;
    rffi_signal_passthrough();
    return Val_unit;
}

CAMLprim value caml_rffi_request_completions(value v_line, value v_cursor_pos) {
    rffi_request_completions(String_val(v_line), Int_val(v_cursor_pos));
    return Val_unit;
}

CAMLprim value caml_rffi_submit_readline_input(value v_input) {
    rffi_submit_readline_input(String_val(v_input));
    return Val_unit;
}
