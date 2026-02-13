# AI Mode Planning

## Overview

Add an AI mode to the REPL, similar to Julia's mode system (`;` for shell, `?` for help, etc.). The AI mode lets the user ask questions about R output, run exploratory commands through an AI agent, and generate scripts from session history.

## Use Cases

- **Interpret output**: "I just ran this regression. What is your interpretation?"
- **Exploratory queries**: "Which of these three columns has the most variance?"
- **Session summarization**: "Write the main code I wrote to a script/rmarkdown file"

## Architecture

### Context Management

Extend the existing SQLite history database to capture R output alongside commands:

```sql
CREATE TABLE history (
    id INTEGER PRIMARY KEY,
    session_id TEXT,
    timestamp REAL,
    input TEXT,
    output TEXT,
    output_lines INT
);
```

Don't dump everything into the AI's context. Instead, expose retrieval tools and let the AI pull what it needs:

- `get_recent(n)` — last N input/output pairs (output truncated to ~200 lines each)
- `search_history(keyword)` — full-text search over inputs and outputs
- `get_environment()` — runs `ls.str()` to see current R state
- `run_r(code)` — execute R in a sandboxed fork and return output

### Integration Approach

**Phase 1 — CLI subprocess**: Shell out to `claude -p` with recent outputs baked into the prompt. No tool use, no MCP. Covers "interpret this" immediately.

**Phase 2 — MCP server**: Build an MCP server that exposes the retrieval tools above. Run a coding agent (Claude Code or other) as a subprocess that connects to the MCP server. The agent gets access to R session state through standard MCP tools and gets file editing / bash capabilities from its own tooling.

```
┌─────────────┐
│   REPL       │
│  (host)      │──── spawns ────▶  coding agent (subprocess)
│              │                        │
│  R session   │◀── MCP (stdio) ──── MCP server binary
│  SQLite DB   │     run_r, history     (spawned by agent)
└─────────────┘
```

For read-only tools (`get_history`, `search_outputs`), the MCP server reads SQLite directly. For `run_r`, the MCP server talks back to the REPL process via unix socket / named pipe, since R runs in the REPL's process.

### UI Rendering

Only the prompt box gets re-rendered. AI responses render into the output area above the prompt box, same as R output, possibly with a different color to distinguish AI text. The prompt shows `ai>` when in AI mode.

For Phase 1 (CLI subprocess), capture stdout and feed it through the existing rendering pipeline. For Phase 2 (MCP + full agent), same approach — the agent's output stream gets rendered as output chunks in the prompt box.

## Sandboxing

AI-executed R commands should run in a sandbox to avoid requiring user approval for every command.

### Approach: Fork + Seatbelt

1. **Fork the R process** — `fork()` gives instant copy-on-write of the R environment. No serialization, near-zero latency. The child has the exact same R memory state.

2. **Apply macOS seatbelt** — Run the forked child under `sandbox-exec` with a restrictive profile:
   - Deny network access
   - Deny file writes outside tmpdir
   - Deny process execution (`system()` calls blocked)
   - Apply rlimits: CPU time cap (~30s), memory cap, file size cap

3. **Capture output, discard child** — The child runs the R command, output is captured, child exits. Parent R session is completely untouched.

This is the same approach OpenAI Codex uses for macOS sandboxing (seatbelt profiles). `sandbox-exec` has been "deprecated" since ~2016 but still works on macOS 15.4, is used by Apple's internal systems, and has no CLI replacement. Risk of removal is low since App Sandbox depends on the same kernel infrastructure.

### "Accept changes" model

The sandbox is read-only by design. The AI explores in the fork, reports findings as text. If the user wants to apply a mutation (e.g., create a new variable), they run the command themselves in the real session, or explicitly approve the AI running it outside the sandbox.

### Limitations

- R isn't fully fork-safe around file descriptors, database connections, and threaded packages. Fine for pure computation, needs care around connections.
- `sandbox-exec` is macOS-only. Linux deployment would use seccomp/Landlock instead.
- Overriding R functions (`system <- function(...) stop("disabled")`) adds defense-in-depth but isn't bulletproof — R has many escape hatches.

## Mode System Design

Modes follow the Julia REPL pattern. Each mode has its own input buffer, history, and prompt. Backspace on empty input returns to normal mode.

| Mode    | Trigger | Prompt     | Backend              |
|---------|---------|------------|----------------------|
| R       | default | `>`        | R FFI                |
| stdin   | from R  | `input>`   | R readline callback  |
| shell   | `;`     | `shell>`   | `system()` via R     |
| AI      | `\`     | `ai>`      | Claude API / CLI     |

Each mode carries its own `lines`/`cursor_pos`/history state so half-typed input is preserved across mode switches.

## Open Questions

- What token/cost budget is acceptable per AI invocation?
- Should AI conversation persist across mode switches within a session?
- Should the AI mode support images (plots)? Would require rendering or opening externally.
- MCP server in OCaml (reuse existing FFI/SQLite bindings) or Python (faster to prototype)?
- Streaming rendering: show tokens as they arrive, or wait for complete response?
