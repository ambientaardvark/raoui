# AI Mode Planning

## Overview

Add an AI mode to the REPL, similar to Julia's mode system (`;` for shell, `?` for help, etc.). The AI mode lets the user ask questions about R output, run exploratory commands through an AI agent, and generate scripts from session history.

## Use Cases

- **Interpret output**: "I just ran this regression. What is your interpretation?"
- **Exploratory queries**: "Which of these three columns has the most variance?"
- **Session summarization**: "Write the main code I wrote to a script/rmarkdown file"

## Status

- ✅ **Phase 1** — `ai>` mode (`:` trigger) talks to `claude -p`, output streamed.
- ✅ **MCP server** — in-process loopback-HTTP (`cohttp-eio`); tools pre-approved via `--allowedTools`.
- ✅ **get_history** — recent interactions, scoped to the current session.
- ✅ **run_r** — sandboxed fork+seatbelt eval of the live session (read-only).
- ✅ **suggest_code** — effectful code dropped into the user's prompt to run.
- ✅ **Tool-call split** — tool turns render a dim `→ tool` line; narration suppressed.
- ✅ **Persistent conversation** — one claude session per raoui run (`--session-id` / `--resume`); `/new` resets it.
- ✅ **Markdown rendering** — AI prose rendered via `cmarkit` → terminal styles, wrapped at live width (`md_layout` / `md_tty`).
- ✅ **search_history** — keyword search over this session's command inputs and output text.
- ⬜ **Fork hardening** — multithreaded-fork allocator hazard accepted for now, tracked in [#32](https://github.com/ambientaardvark/raoui/issues/32).

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

Don't dump everything into the AI's context. Instead, expose MCP tools and let the AI pull or compute what it needs:

- `get_history(limit)` — recent input/output pairs, scoped to the current session ✅
- `run_r(code)` — evaluate R in a sandboxed fork of the live session (read-only) ✅
- `suggest_code(code)` — place effectful R code in the user's prompt for them to run ✅
- `search_history(keyword)` — substring search over this session's inputs and output text ✅

`get_environment()` (an `ls.str()` snapshot) from the original plan is dropped — `run_r` subsumes it: the AI can run `ls.str()` itself, and the sandbox fork sees the live session copy-on-write.

### Integration Approach

**Phase 1 — CLI subprocess** *(done)*: Shell out to `claude -p` per query, streaming its `stream-json` output back through a fourth Eio fiber. Built-in tools and all globally-configured MCP servers are disabled (`--tools ""`, `--strict-mcp-config`); a focused `--system-prompt` replaces Claude Code's heavy default. Started with no baked context — the AI answers purely from the user's prompt text.

**Phase 2 — MCP server** *(largely done)*: Expose the tools above to the `claude -p` subprocess via MCP. Because the tools must operate on raoui's *live* state (fork its R worker, read its already-open SQLite handle), the MCP server lives **in-process in raoui**, not as a separate binary the agent spawns. raoui serves a minimal MCP streamable-HTTP endpoint on a loopback port (`cohttp-eio`); claude connects out to it via `--mcp-config`, with the tools pre-approved through `--allowedTools` so the non-interactive `-p` run never blocks on a permission prompt. Done: `get_history`, `search_history`, `run_r`, `suggest_code`.

```
┌──────────────────────┐
│   raoui (host)       │
│                      │── spawns ──▶  claude -p (subprocess)
│  R session           │                     │
│  SQLite DB           │◀─ MCP over ─────────┘
│  in-proc MCP server ─┼─  loopback HTTP
│  (cohttp-eio fiber)  │   get_history, search_history, run_r, suggest_code
└──────────────────────┘
```

Hosting the server inside raoui collapses the old plan's two-hop design: `get_history` reads the in-process SQLite handle directly, and `run_r` forks raoui's R worker directly — no separate server binary and no unix-socket bridge back to the REPL. `suggest_code` pushes its code to the frontend as an `Ai_suggestion` chunk on the same Eio stream the AI's output travels on.

### Execution model: explore vs. change

The AI splits R work by side effects, which maps onto the two execution tools:

- **Read-only / exploration → `run_r`.** Sandboxed (fork + seatbelt), so the AI runs it freely with no approval — nothing it does persists or escapes. This is the agent's exploration loop: run, observe, refine.
- **Effectful / persistent → `suggest_code`.** Code that should change the session (assignments to keep, file writes, plots) can't be sandboxed (the whole point is to persist), so the AI doesn't run it — it drops the code into the user's prompt, and the user running it is the approval.

The sandbox doubles as a safety net: even if the AI misjudges and runs effectful code via `run_r`, the effect dies with the fork, so "AI runs it" is always safe. `suggest_code` is only for changes the user actually wants to keep.

### Conversation continuity

The backend mints a session UUID per raoui run: the first `ai>` query passes `--session-id`, later ones `--resume`, so claude replays the conversation and follow-ups keep context. `/new` (or `/reset`, `/clear`) starts a fresh session. The cache is server-side and time-keyed (5-min TTL), so continuity costs the usual growing-context tokens per turn but the per-call process/MCP init is the only thing a long-lived process would save.

### UI Rendering

Only the prompt box gets re-rendered. AI responses render into the output area above the prompt box, same as R output, possibly with a different color to distinguish AI text. The prompt shows `ai>` when in AI mode.

The AI's `stream-json` events are parsed by block type (`Ai_backend.assistant_chunks`) into response chunks the event loop renders:

- **`Ai_output`** — user-facing assistant text → rendered as prose (markdown, see below).
- **`Ai_tool_call name`** — emitted per `tool_use` block; renders as a dim `→ run_r` line. A message that contains a `tool_use` is a "tool turn": its narration text blocks are suppressed (so "I'll now check…" preambles don't scroll), and only the compact indicator shows. The indicator is a deliberately simple hook to refine per-tool later.
- **`Ai_suggestion code`** — from `suggest_code`; stashed and dropped into the input prompt when the response finalizes.

**Markdown rendering** *(done)*. AI prose comes back as markdown. No turnkey markdown→ANSI renderer exists in opam (no equivalent of Glow/`rich`), so we parse with `cmarkit` (pure OCaml, CommonMark, extensible renderer API) and map the AST onto terminal styles — headings→bold, emphasis→italic/dim, code spans/blocks→color, lists→bullets. It's a dedicated `Output_markdown` repl_output, laid out by `md_layout` and emitted as styled spans by `md_tty`, re-rendered and wrapped at the live terminal width on each render. Fenced ```r (and untagged) code blocks are run through the existing `R_highlight` for real R syntax highlighting; inline code spans stay a single flat color (conventional, since they're often a bare identifier or filename rather than a full expression).

## Sandboxing *(implemented)*

`run_r` runs AI code in a sandbox so it needs no per-command approval. Both halves were proven in isolation first with standalone spikes (the seatbelt profile and the R-fork COW behaviour, commits `61e30ac` / `2f20620`) before wiring into the FFI.

### Approach: Fork + Seatbelt (on the R worker thread)

1. **Fork the R process** — `fork()` gives instant copy-on-write of the R environment. The child has the exact same R memory state, so the AI's code sees the live session's variables for free. The fork happens on the R worker thread (where R's eval context lives).

2. **Apply macOS seatbelt — in-process, not `sandbox-exec`.** Because we fork *without* re-exec (to keep the COW R memory), the child calls `sandbox_init()` directly with an SBPL profile. The profile (assembled per call by `build_runr_profile`) is `allow default` then a series of denies — SBPL is last-match-wins, so each `deny` overrides the default and a following scoped `allow` overrides the deny:
   - Deny network (`network*`)
   - Deny file writes except a scratch dir + `/dev/null` (`file-write*`)
   - Deny process execution (`process-exec*`) — blocks `system()`
   - **Deny file reads, then re-allow only R's machinery + the working dir** (`file-read*`). This closes the exfiltration path: `run_r`'s whole job is to feed output back to the model, so an unrestricted read would let the AI `readLines("~/.ssh/id_rsa")` straight into its (API-bound) response. The read allow-list is generated at fork time from the *live* R — `R.home()`, every `.libPaths()` entry, `tempdir()`, `getwd()` — plus a fixed set of system roots (`/usr`, `/System`, `/Library`, `/opt/homebrew`, `/private/var`, `/etc`, `/dev`) that dyld/locale/timezone need. Secrets elsewhere under `$HOME` (`~/.ssh`, `~/.aws`, keychains, `.Renviron`) are denied. This is cheap because the child forks an *already-initialized* R: the base session is in memory COW, so the read paths only need to cover what AI code triggers *after* the fork (lazy `library()` loads, locale/tz data, cwd data files) — anything else is a clean `EPERM` and `run_r` fails safe.
   - **Resource guards** layered on the sandbox so a misbehaving child can't wedge or thrash the host:
     - `setrlimit(RLIMIT_CPU, 10s)` — a CPU-bound loop dies with `SIGXCPU`.
     - **Wall-clock watchdog (30s)** — the parent drains the pipe via `poll()` against a deadline and `SIGKILL`s the child if it elapses. This catches what `RLIMIT_CPU` *can't*: a child that **blocks** rather than burns CPU (a deadlock, hung I/O, or the #32 fork hazard) — which would otherwise freeze the R worker thread, since the parent runs on it.
     - **Memory cap** via R's own `mem.maxVSize(4096 Mb)`, set in the child before eval. macOS rejects `RLIMIT_AS` (verified: `setrlimit` returns -1), so an OS address-space cap is unavailable; R's vector-heap ceiling instead turns an oversized allocation into a clean R error rather than OS thrash.

   Validated on macOS 26.5: every denial is a clean `EPERM` / non-zero return, **never a `SIGKILL`** from the sandbox itself — so the child's failures are capturable as text and handed back to the AI. `sandbox_init` is deprecated since ~2016 but still works; `sandbox-exec` would have re-exec'd and lost the fork's memory, so the in-process API is the right fit here.

   Smoke-tested end to end: read scoping (`1+1` and `library(Matrix)` work; `/etc/hosts` and a cwd file read; an *existing* `$HOME` file is blocked — EPERM, not a missing file), the memory cap (an 80 MB alloc works, a >4 GB alloc fails with "vector memory limit of 4.0 Gb reached"), the CPU limit (a busy loop dies on SIGXCPU at ~10s), and the watchdog (`Sys.sleep(60)` is killed at 30s with a "timed out" note).

3. **Capture output, discard child** — output is captured by repointing R's `WriteConsoleEx` callback at a pipe in the child; the parent (worker thread) drains the pipe and `waitpid`s. Parent R session is completely untouched (verified: child mutations don't leak, parent keeps evaluating).

### Explore vs. change (realized)

The "read-only sandbox" model is realized as the `run_r` / `suggest_code` split (see Execution model above): the AI explores in the fork via `run_r` and, for changes the user wants to keep, hands the code over via `suggest_code` rather than running it — the user running it is the approval.

### Limitations

- **Multithreaded fork hazard** — the spikes forked a single-threaded process; raoui is multithreaded (Eio + R worker + systhreads). If another thread holds the malloc lock at fork, the child can deadlock on its first allocation. Low probability (worker forks at a quiescent point); now *survivable* rather than fatal thanks to the 30s watchdog, which kills the wedged child so the worker thread recovers. Lowering the probability itself (quiesce threads / `pthread_atfork`) is still tracked in [#32](https://github.com/ambientaardvark/raoui/issues/32).
- R isn't fully fork-safe around file descriptors, DB connections, and threaded packages. Fine for pure computation; the strict write profile means R operations needing temp-file writes can fail.
- macOS-only. Linux deployment would use seccomp/Landlock instead.
- Reads are now deny-default (scoped to R + cwd), but writes/network/exec still use the allow-default-then-deny structure. The read allow-list is a *blocklist's inverse* and could still be widened by a path R reports (e.g. a `.libPaths()` entry that itself sits next to secrets); acceptable given the threat model. A user-configurable extra-read-paths knob (for data outside cwd) is a natural follow-on.
- The memory cap covers R's vector heap (the dominant consumer for data), not arbitrary C-level allocations inside packages; the watchdog backstops the rest. The 10s CPU / 30s wall limits suit quick exploration — a genuinely long read-only computation should go through `suggest_code` instead.

## Mode System Design

Modes follow the Julia REPL pattern. Each mode has its own input buffer, history, and prompt. Backspace on empty input returns to normal mode.

| Mode    | Trigger | Prompt     | Backend              |
|---------|---------|------------|----------------------|
| R       | default | `>`        | R FFI                |
| stdin   | from R  | `input>`   | R readline callback  |
| shell   | `;`     | `shell>`   | `system()` via R     |
| AI      | `:`     | `ai>`      | `claude -p` CLI (persistent session) |

Each mode carries its own `lines`/`cursor_pos`/history state so half-typed input is preserved across mode switches.

## Open Questions

- What token/cost budget is acceptable per AI invocation? (A tool-using turn costs ~$0.04 with the lean system prompt + `--tools ""`; cache is server-side with a 5-min TTL, so spaced-out turns re-pay cache creation.)
- Should the AI mode support images (plots)? Would require rendering or opening externally.
- ~~MCP server in OCaml or Python?~~ Resolved: in-process OCaml (`cohttp-eio`), so tools reach raoui's live R session and SQLite handle directly.
- ~~Should AI conversation persist across queries?~~ Resolved: yes, one claude session per raoui run (`--session-id` / `--resume`), reset with `/new`.
- Streaming rendering: show tokens as they arrive, or wait for complete response? Markdown rendering needs a complete block (can't style a fenced block until its closing fence arrives), so it pulls toward buffering the full response — which also suits `suggest_code` (needs the whole response anyway).
