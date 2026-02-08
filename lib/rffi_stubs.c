#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/threads.h>
#include <string.h>
#include <stdlib.h>
#include "r_bridge.h"
#include "ring_buffer.h"

CAMLprim value caml_rffi_init(value v_r_home) {
    int rc = rffi_init(String_val(v_r_home));
    return Val_int(rc);
}

/* Releases the OCaml runtime lock so the Eio scheduler can
   poll the ring buffer on the main thread while R computes. */
CAMLprim value caml_rffi_eval(value v_code) {
    char *code = strdup(String_val(v_code));
    caml_release_runtime_system();
    int rc = rffi_eval(code);
    caml_acquire_runtime_system();
    free(code);
    return Val_int(rc);
}

CAMLprim value caml_rffi_shutdown(value v_unit) {
    (void)v_unit;
    rffi_shutdown();
    return Val_unit;
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
    char buf[65536];
    uint32_t out_len;

    int rc = rffi_rb_pop(&kind, &flags, buf, sizeof(buf), &out_len);
    if (rc == RB_EMPTY) {
        CAMLreturn(Val_int(0)); /* None */
    }

    v_payload = caml_alloc_string(out_len);
    memcpy(Bytes_val(v_payload), buf, out_len);

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
