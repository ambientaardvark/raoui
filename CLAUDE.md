# RAOUI

An interactive R REPL written in OCaml. Executes R code via the Ark kernel using the Jupyter/ZMQ protocol.

## Build & Run

```
dune build
dune exec raoui
dune test
```

## Architecture

Follows an Elm-style MVU (Model-View-Update) pattern with Eio fibers for concurrency.

**Key modules:**
- `Frontend_types` - Model definition (lines, cursor, history, terminal state)
- `Update` - Handles input events, updates model state
- `View` - Renders model to terminal operations
- `Terminal_ops` - Abstraction over ANSI escape sequences
- `Tty_listener` - Parses raw TTY input into key events
- `Backend` - ZMQ communication with Ark kernel (execute_request/iopub)

**Event loop (`main.ml`):** Races three fibers—user input, backend response, and terminal resize—then dispatches to the appropriate handler.

**Coordinate systems:** Internal coordinates (line index, column) differ from terminal coordinates (row, column) due to line wrapping. Conversion functions in `Frontend_types` handle this.
