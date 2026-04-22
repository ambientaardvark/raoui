# Raoui: a modern R console for busy statisticians

Raoui is a personal project and terminal application for interactive work in R. It is built in OCaml with a small C bridge into the R runtime, with a focus on responsive terminal UX, careful control over rendering, and experimentation with REPL features that are awkward to build on top of the standard R console.

### Why build Raoui?

R in the terminal is useful for quick analysis, portability, and remote work, but the default console is fairly bare-bones. Tools like `radian` improve the experience, though I wanted lower-level control over rendering, responsiveness, and integration with terminal-native features. Raoui is an attempt to build a smoother terminal R workflow without giving up performance or flexibility.

### Features

- Syntax highlighting

<img src="docs/images/syntax_highlighting.png" width="600" alt="Syntax highlighting">

- Tab autocomplete

<img src="docs/images/autocomplete.png" width="600" alt="Tab autocomplete">

- Inline graphs in the terminal using Kitty or ITerm

<img src="docs/images/inline_plots.png" width="600" alt="Inline plots">

- Fast performance and startup
- Multi-line editing
- Smooth integration with the VS Code extension
- Full unicode support
- Type your next command while the current command is finishing
- Shell mode to enter bash commands directly
- Magic commands like in IPython (use these to load a file with the Finder app, or insert unicode math symbols)

### Architecture

Unlike a traditional REPL, Raoui runs the UI and interpreter on separate OS threads. The interpreter side uses C to link against the R runtime at run time and communicates asynchronously with the UI through a ring buffer. This keeps startup fast, lets the UI remain responsive while code is executing, and provides a base for features like inline plotting and background completion requests. The UI itself is built as an event-driven runtime with Elm-style state updates and views.

### Development Status

The project is under active development, but it is already a fully working codebase with automated tests.

### Build instructions

You will need OCaml 5.0+, a C compiler, and a local R installation. The main development target is macOS. Build with

```bash
opam install --deps-only .
dune exec raoui
```

Run the test suite with

```bash
dune runtest
```
