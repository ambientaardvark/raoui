# Streaming R Output Over Raw C FFI: Implementation Plan

## Goal
Replace the current kernel/ZMQ output path with an in-process, chunk-streaming C FFI path that:
- delivers output incrementally (for progress bars and live logs),
- is safe under high output volume,
- integrates cleanly with OCaml/Eio,
- can evolve to non-text payloads (images, richer events).

## Non-Goals (for initial version)
- Full parity with every Jupyter message type.
- Perfect replay of all historical output under unbounded volume.
- Complex multi-producer support.

## Architecture Overview
- R runtime embedded via `libR` and raw `dlopen`/`dlsym`.
- `ptr_R_WriteConsoleEx` callback receives stream chunks (`const char *s, int len, int otype`).
- `ptr_R_FlushConsole` callback signals when partial output should be rendered (important for progress bars).
- Callback pushes chunks into a C-owned ring buffer.
- OCaml calls `rffi_eval` on an `Eio.Unix.run_in_systhread`, freeing the Eio main fiber to poll the ring buffer concurrently.
- OCaml maps chunk kinds to existing `backend.ml` response types.

## Why Ring Buffer
- Fixed memory footprint.
- Fast producer/consumer operations.
- Works well for callback hot path (minimal blocking/allocations).
- Supports terminal-like behavior with controlled loss.

## Buffer Policy (Decided)
Use **drop-oldest** when full.

Rationale:
- Matches terminal UX: most recent output is most relevant.
- Prevents producer-side blocking in callbacks.
- Keeps live progress output responsive.

Policy details:
1. Store framed messages (not raw stream bytes).
2. If full, evict oldest complete messages until new message fits.
3. If one incoming message is larger than total capacity, store a truncated message and mark it truncated.
4. Track dropped metrics (`dropped_messages`, `dropped_bytes`).
5. Emit periodic synthetic notice message (optional v1.1): `output truncated: ...`.

## Message Frame Format
Use binary-safe frames to support future payload types.

Suggested frame schema:
- `kind` (`uint8_t`)
- `flags` (`uint8_t`) (e.g., truncated)
- `len` (`uint32_t`)
- `payload[len]`

Suggested `kind` enum:
- `RB_MSG_STDOUT`
- `RB_MSG_STDERR`
- `RB_MSG_RESULT`
- `RB_MSG_R_ERROR`
- `RB_MSG_INTERNAL_ERROR`
- `RB_MSG_DONE`
- `RB_MSG_IMAGE` (future)
- `RB_MSG_NOTICE` (optional)

Mapping from R callback:
- `otype == 0` -> `RB_MSG_STDOUT`
- `otype != 0` -> `RB_MSG_STDERR` (or `RB_MSG_R_ERROR` depending on source)

## R Eval Strategy (Decided)
Use `R_ParseVector` + `R_tryEval` instead of `R_ParseEvalString`.

Rationale:
- `R_ParseEvalString` calls `longjmp` on parse/eval errors, which crashes the process.
- `R_tryEval` takes an `int *errorOccurred` out-param and returns normally on error.
- This lets us reliably emit `RB_MSG_R_ERROR` and `RB_MSG_DONE` signals.

Eval flow:
1. `R_ParseVector(code_sexp, -1, &parse_status, R_NilValue)` → parsed expression list + status.
2. Check `parse_status` for `PARSE_OK` / `PARSE_ERROR` / `PARSE_INCOMPLETE`.
3. Loop over parsed expressions, calling `R_tryEval(expr, R_GlobalEnv, &err)` for each.
4. If `err != 0`, push `RB_MSG_R_ERROR` with the error message.
5. For the last expression's result, push `RB_MSG_RESULT` via `Rf_asChar` in C.
6. Push `RB_MSG_DONE` after all expressions complete (or after error).

Additional symbols to load via `dlsym`:
- `R_ParseVector`
- `R_tryEval`
- `R_NilValue`
- `Rf_mkString` (to wrap code string into SEXP)
- `Rf_protect` / `Rf_unprotect` (GC protection)
- `VECTOR_ELT`, `Rf_length` (to iterate parsed expression list)

## C Module Organization
Create dedicated files in `c_ffi`:
- `ring_buffer.h`
- `ring_buffer.c`
- `r_bridge.h` (optional, exported FFI API)
- `r_bridge.c` (or continue in `load_r.c` then refactor)

## C API (FFI-Friendly)
### Ring buffer lifecycle
- `int rb_init(size_t capacity_bytes)`
- `void rb_deinit(void)`
- `void rb_reset(void)`

### Producer (R callback side)
- `int rb_push(uint8_t kind, uint8_t flags, const char *data, uint32_t len)`

### Consumer (OCaml side)
- `int rb_has_data(void)`
- `int rb_pop(uint8_t *kind, uint8_t *flags, char *out, uint32_t out_cap, uint32_t *out_len)`
- `uint64_t rb_dropped_messages(void)`
- `uint64_t rb_dropped_bytes(void)`

### Eval/control
- `int rffi_init(const char *r_home)`
- `int rffi_eval(const char *code)`
- `void rffi_shutdown(void)`

Notes:
- `rb_pop` should return status codes (`OK`, `EMPTY`, `TRUNCATED_COPY`, `ERROR`).
- Keep callback path non-blocking and minimal.
- Copy callback input bytes immediately using provided `len`.

## R Callbacks to Hook
- `ptr_R_WriteConsole` — set to `NULL` (disables legacy callback).
- `ptr_R_WriteConsoleEx` — push `RB_MSG_STDOUT`/`RB_MSG_STDERR` to ring buffer.
- `ptr_R_FlushConsole` — push `RB_MSG_FLUSH` (or set a flush flag) so OCaml knows to render partial output immediately. Critical for progress bars.
- `ptr_R_ShowMessage` — push `RB_MSG_STDERR` (catches error/warning messages not routed through WriteConsoleEx).

## Concurrency Model
Architecture with `Eio.Unix.run_in_systhread`:
- **Producer**: R callback fires on the systhread running `rffi_eval`, pushes to ring buffer.
- **Consumer**: Eio main fiber polls ring buffer concurrently from the OCaml runtime thread.
- These are truly concurrent (different OS threads) → **mutex is required**.

Start with mutex + clear invariants. SPSC lock-free ring is an option later if profiling shows contention.

## OCaml/Eio Integration Plan
### OCaml FFI surface
Expose C stubs:
- `init : string -> unit`
- `eval : string -> unit`
- `pop_chunk : unit -> (kind * flags * bytes) option`
- `dropped_stats : unit -> int64 * int64`
- `shutdown : unit -> unit`

### Event loop behavior
In `backend.ml`:
- Replace ZMQ backend wholesale (no feature flag).
- `eval` runs on `Eio.Unix.run_in_systhread` so it doesn't block the Eio scheduler.
- Meanwhile, Eio main fiber polls `pop_chunk` in a loop.
- Convert kinds to existing `response_chunk`:
  - `RB_MSG_STDOUT` -> `Stdout`
  - `RB_MSG_RESULT` -> `Result`
  - `RB_MSG_R_ERROR` / `RB_MSG_STDERR` -> `R_error` (or separate path)
  - `RB_MSG_DONE` -> `Done`

### Wakeup strategy
Initial:
- lightweight polling with `Eio.Fiber.yield ()` and short sleep/backoff.

Later:
- add explicit wakeup fd/eventfd pipe from C producer to integrate with Eio readiness.

## Concrete Implementation Steps
1. Add `ring_buffer.h/.c` with framed-message ring and drop-oldest policy.
2. Add tests or a small C harness for ring behavior:
   - wrap-around correctness,
   - full-buffer eviction,
   - oversize message truncation,
   - dropped stats accuracy.
3. Refactor `load_r.c` into bridge API (`rffi_init`, `rffi_eval`, callback wiring).
   - Replace `R_ParseEvalString` with `R_ParseVector` + `R_tryEval`.
   - Load additional symbols (`R_ParseVector`, `R_tryEval`, `R_NilValue`, `Rf_mkString`, `Rf_protect`/`Rf_unprotect`, `VECTOR_ELT`, `Rf_length`).
   - Hook `ptr_R_FlushConsole` and `ptr_R_ShowMessage` in addition to `ptr_R_WriteConsoleEx`.
   - Remove standalone `main()`.
4. Wire callbacks to push chunks into ring buffer with `len` and `otype` mapping.
5. Add completion signaling (`RB_MSG_DONE`) around eval boundaries.
6. Add OCaml C stubs and Dune wiring.
7. Add OCaml-side decoder + mapping to `response_chunk`.
8. Integrate into `backend.ml` as wholesale replacement.
9. Validate with interactive cases:
   - simple print,
   - long output,
   - progress bar updates,
   - errors/warnings,
   - cancellation/interruption.
10. Remove/retire old ZMQ path when stable.

## Risk Areas and Mitigations
- Callback reentrancy/thread assumptions:
  - keep callback work tiny; avoid allocations where possible.
- Message boundary confusion:
  - preserve callback chunking; do not force newline-based parsing.
- Data loss under spam output:
  - explicit drop metrics + optional user-visible truncation notice.
- Pointer lifetime bugs:
  - always copy incoming callback bytes immediately.
- R eval errors (longjmp):
  - use `R_tryEval` exclusively; never `R_ParseEvalString`.

## Milestone Plan
### Milestone 1 (Core)
- Ring buffer module + C harness passing.
- R bridge with `R_tryEval` replacing `R_ParseEvalString`.
- R callbacks push stdout/stderr chunks.
- OCaml can pop and display streaming output.

### Milestone 2 (Execution Semantics)
- `eval` emits `Result`/`R_error`/`Done` consistently.
- `backend.ml` state machine mirrors current behavior.

### Milestone 3 (Robustness)
- dropped-output notices,
- interruption handling,
- optional wakeup fd integration,
- image/binary payload path.

## Resolved Decisions
- `otype != 0`: map to `RB_MSG_STDERR` initially. Distinguish from `RB_MSG_R_ERROR` (which comes from `R_tryEval` failure) at the C level.
- `Result` computed in C via `Rf_asChar` (simpler, avoids parsing R's print format in OCaml).
- Buffer capacity: start at 1 MB. No real downside to generous allocation for a desktop REPL.
- No feature flag; replace ZMQ backend wholesale on a branch.

## Open Decisions
- Poll interval/backoff values before wakeup-fd work lands.
- Exact `ptr_R_FlushConsole` signaling mechanism (dedicated message kind vs flag on previous message).
