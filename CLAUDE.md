# RAOUI

An interactive R REPL written in OCaml. Embeds R directly via FFI (dynamically loading libR).

## Build & Run

```
dune build
dune exec raoui
dune test
```

## Architecture

Follows an Elm-style MVU (Model-View-Update) pattern with Eio fibers for concurrency.

**Frontend:**
- `Frontend_types` - Model definition (lines, cursor, history, terminal state)
- `Update` - Handles input events, updates model state
- `View` - Renders model to terminal operations
- `Terminal_ops` / `Escape_seq` - ANSI escape sequence abstraction
- `Tty_listener` - Parses raw TTY input into key events
- `Unicode_string` - UTF-8 string type with grapheme cluster and display-width awareness
- `R_lexer` / `Syntax` - Sedlex-based R lexer and syntax highlighting
- `History` - SQLite-backed command history with fuzzy prefix search (`~/.raoui_history.db`)

**Backend (FFI):**
- `Ffi_backend` - Submits R code, polls a ring buffer for results, handles output suppression for background submissions and passthrough mode for `system()` calls
- `Rffi` - OCaml external declarations for the C FFI
- `rffi_stubs.c` - OCaml↔C glue; releases the OCaml runtime lock during blocking calls so Eio can keep scheduling
- `c_ffi/load_r.c` - Dynamically loads libR (`dlopen`/`dlsym`), initializes R, runs eval in a worker thread, captures stdout/stderr via callbacks
- `c_ffi/ring_buffer.c` - Pthread-mutex-protected ring buffer for thread-safe R→OCaml message passing

**Event loop (`main.ml`):** Races three Eio fibers—user input, backend response, and terminal resize—then dispatches to `Update`. Handles passthrough mode: when R calls `system()`, raw mode is suspended so the subprocess can control the terminal directly.

**Coordinate systems:** Internal coordinates (line index, grapheme column) differ from terminal coordinates (row, column) due to line wrapping and variable-width characters. Conversion functions in `Frontend_types` handle this.

## Style Guide

Based on [OCaml Programming Guidelines](https://ocaml.org/docs/guidelines).

**Structure:**
- Break programs into small functions. If a pattern match clause gets long, extract it to its own function.
- Solve problems with types and pattern matching. Define explicit sum types instead of encoding data as booleans or integers.
- Avoid catch-all `| _ ->` clauses in pattern matches — list constructors explicitly so the compiler warns when new ones are added.

**Naming & modules:**
- Use underscores for word separation, never camelCase (capitals are reserved for constructors/modules).
- Avoid `open` directives; use qualified notation (e.g., `List.map`, `String.length`).

**Records:**
- Comment what every field does.

**Error handling:**
- Prefer `option`/`result` types over exceptions.

**Other:**
- Use library iterators (`List.map`, `List.fold_left`) rather than reinventing them.
- Prefer immutable data structures where possible.
