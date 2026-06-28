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

**Phase 1 — CLI subprocess** *(done)*: Shell out to `claude -p` per query, streaming its `stream-json` output back through a fourth Eio fiber. Built-in tools and all globally-configured MCP servers are disabled (`--tools ""`, `--strict-mcp-config`); a focused `--system-prompt` replaces Claude Code's heavy default. Started with no baked context — the AI answers purely from the user's prompt text.

**Phase 2 — MCP server** *(in progress)*: Expose the retrieval tools above to the `claude -p` subprocess via MCP. Because the tools must operate on raoui's *live* state (fork its R worker, read its already-open SQLite handle), the MCP server lives **in-process in raoui**, not as a separate binary the agent spawns. raoui serves a minimal MCP streamable-HTTP endpoint on a loopback port (`cohttp-eio`); claude connects out to it via `--mcp-config`, with the read-only tools pre-approved through `--allowedTools` so the non-interactive `-p` run never blocks on a permission prompt. Implemented so far: `get_history` (session-scoped). Still to do: `search_history`, sandboxed `run_r`.

```
┌──────────────────────┐
│   raoui (host)       │
│                      │── spawns ──▶  claude -p (subprocess)
│  R session           │                     │
│  SQLite DB           │◀─ MCP over ─────────┘
│  in-proc MCP server ─┼─  loopback HTTP
│  (cohttp-eio fiber)  │   get_history, (run_r)
└──────────────────────┘
```

Hosting the server inside raoui collapses the old plan's two-hop design: read-only tools (`get_history`, `search_history`) read the in-process SQLite handle directly, and `run_r` can fork raoui's R worker directly — no separate server binary and no unix-socket bridge back to the REPL.

### UI Rendering

Only the prompt box gets re-rendered. AI responses render into the output area above the prompt box, same as R output, possibly with a different color to distinguish AI text. The prompt shows `ai>` when in AI mode.

For Phase 1 (CLI subprocess), capture stdout and feed it through the existing rendering pipeline. For Phase 2 (MCP + full agent), same approach — the agent's output stream gets rendered as output chunks in the prompt box.

**TODO — render AI markdown in the terminal.** AI responses come back as markdown but are currently shown as raw text. No turnkey markdown→ANSI renderer exists in opam (no equivalent of Glow/`rich`). Plan: parse with `cmarkit` (pure OCaml, CommonMark, extensible renderer API) and write a small backend that maps the AST onto our existing `Terminal_ops` styles/theme — headings→bold, emphasis→italic/dim, code spans→color, lists→bullets. Special-case fenced ```r blocks through the existing `R_highlight` rather than a flat code style. Open sub-question: markdown rendering needs a complete block (can't style a fenced block until its closing fence arrives), so this pushes toward buffering the full response before rendering rather than the current chunk-by-chunk streaming — which also aligns with the "place suggested code in the prompt" feature that needs the whole response anyway. `omd` is the fallback parser if `cmarkit` doesn't fit.

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
| AI      | `:`     | `ai>`      | `claude -p` CLI      |

Each mode carries its own `lines`/`cursor_pos`/history state so half-typed input is preserved across mode switches.

## Open Questions

- What token/cost budget is acceptable per AI invocation?
- Should AI conversation persist across mode switches within a session?
- Should the AI mode support images (plots)? Would require rendering or opening externally.
- ~~MCP server in OCaml or Python?~~ Resolved: in-process OCaml (`cohttp-eio`), so tools reach raoui's live R session and SQLite handle directly.
- Streaming rendering: show tokens as they arrive, or wait for complete response? (See the markdown-rendering TODO above — markdown styling and the place-in-prompt feature both pull toward buffering the full response.)
