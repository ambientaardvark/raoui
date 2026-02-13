# Data Viewer Spec

## 1. Design Goals

### 1.1 Core Purpose

A standalone data viewer for an OCaml-based R REPL. The viewer displays tabular R data (data frames, tibbles, data.tables, Arrow tables) in a native GUI, with lazy loading to support datasets close to machine memory capacity.

### 1.2 Constraints

- **Memory footprint is critical.** A user with a 30GB data frame on a 32GB machine cannot afford a copy. The viewer must work from the original data or a single-copy serialization, never a full duplication.
- **Startup time matters.** The viewer process should launch in under 200ms and be ready to display data. After first launch, subsequent `view()` calls reuse the warm process (new tab/window, no relaunch).
- **Format on the backend, display on the frontend.** The viewer receives pre-formatted strings. It never interprets R types, never links against R, and never calls into the R runtime. This is the key architectural boundary.
- **The protocol is the product.** The viewer GUI is a thin, replaceable shell. The protocol between REPL and viewer is the stable interface. Multiple viewer implementations (Swift/AppKit, GTK4, TUI) can coexist.

### 1.3 Platform Priority

1. macOS (Swift/AppKit) — primary target
2. Terminal UI (ratatui or similar) — high-value fallback for SSH/headless
3. Linux (GTK4/libadwaita) — if demand warrants
4. Windows (WinUI) — lowest priority

### 1.4 Scope: What This Is Not

- Not a spreadsheet. No formulas, no computed columns, no drag-fill. Supports single-cell spot edits only.
- Not a full IDE component. No variable explorer, no plot pane, no console integration.
- No built-in sorting or filtering. The viewer displays data in the order it exists in the R object. Sort and filter operations belong in the REPL — the user writes `df[order(x),]` or `df[carrier == "UA",]` and views the result. This keeps the viewer simple and avoids reimplementing data manipulation badly.
- Not a replacement for code-based data manipulation. The viewer complements the REPL, it does not replace it.


## 2. Protocol Design

### 2.1 Transport

Unix domain socket. The REPL spawns the viewer as a child process on first `view()` call, passing the socket path as a command-line argument. The viewer stays alive across multiple view commands, opening new tabs/windows for each.

Messages are newline-delimited JSON. No framing protocol, no Jupyter envelope, no ZeroMQ. Each message is a single JSON object followed by `\n`.

### 2.2 Connection Lifecycle

```
REPL                                    Viewer
  │                                       │
  │──spawn(viewer --socket /tmp/rv.sock)──│
  │                                       │
  │◄──────── { "ready": true } ───────────│
  │                                       │
  │─── open_table { id, schema } ────────►│
  │                                       │
  │◄── get_data { id, row_range } ────────│
  │─── data { id, columns } ─────────────►│
  │         ...                           │
  │◄── close_table { id } ────────────────│
  │                                       │
```

The viewer is the **client** (it requests data). The REPL is the **server** (it responds with data). This matches the natural data flow: the viewer asks for what it needs to display, the REPL provides it.

### 2.3 Messages: REPL → Viewer

#### `open_table`

Sent when the user calls `view(df)`. Opens a new tab/window in the viewer.

```json
{
  "method": "open_table",
  "params": {
    "table_id": "t_001",
    "title": "flights",
    "editable": true,
    "schema": {
      "num_rows": 336776,
      "columns": [
        {
          "name": "year",
          "display_type": "integer",
          "r_type": "integer",
          "label": null,
          "na_count": 0
        },
        {
          "name": "carrier",
          "display_type": "string",
          "r_type": "character",
          "label": "Airline carrier code",
          "na_count": 0
        },
        {
          "name": "dep_time",
          "display_type": "integer",
          "r_type": "integer",
          "label": null,
          "na_count": 8255
        }
      ]
    }
  }
}
```

**`display_type` enum:** `string`, `integer`, `number`, `boolean`, `date`, `datetime`, `factor`, `labelled`, `blob`, `unknown`. This controls alignment, coloring, and formatting in the viewer. It is distinct from `r_type`, which is informational. `labelled` indicates a `haven_labelled` vector where value labels should be rendered as the primary cell text.

**`label`:** Optional column-level label (from R's `label` attribute), displayed as subtitle in the column header.

#### `data_response`

Response to a `get_data` request from the viewer.

```json
{
  "method": "data_response",
  "params": {
    "table_id": "t_001",
    "request_id": "req_42",
    "row_offset": 5000,
    "columns": [
      {
        "name": "year",
        "values": ["2013", "2013", "2013", "..."],
        "na_mask": [false, false, false, "..."]
      },
      {
        "name": "satisfaction",
        "values": ["Strongly Agree", "Neutral", null, "..."],
        "secondary": ["1", "3", null, "..."],
        "na_mask": [false, false, true, "..."]
      }
    ]
  }
}
```

Values are always strings, pre-formatted by the backend. `null` in `values` combined with `true` in `na_mask` indicates NA. The viewer renders NA cells with a distinct style (e.g., grayed-out "NA" text).

**`secondary`:** Optional. Present on `labelled` and `factor` columns. Contains the underlying code/level as a string. The viewer shows `values` as the primary cell text and `secondary` on hover or as a dimmed suffix, depending on column width. For the `satisfaction` example above, the cell might render as "Strongly Agree" with "1" shown on hover.

**All columns are always included.** Column subsetting adds protocol complexity for negligible benefit — even 100 columns × 256 rows is a few KB of strings.

#### `update_table`

Sent when the underlying R object has changed (e.g., user modifies `df` in the REPL).

```json
{
  "method": "update_table",
  "params": {
    "table_id": "t_001",
    "schema": { "..." },
    "invalidate": true
  }
}
```

When `invalidate` is true, the viewer discards its page cache and re-fetches visible rows. The schema may have changed (columns added/removed/retyped).

#### `close_table`

Sent when the R object is garbage collected or the REPL session ends.

```json
{
  "method": "close_table",
  "params": { "table_id": "t_001" }
}
```

#### `blob_response`

Future extension for inline images and rich cell content.

```json
{
  "method": "blob_response",
  "params": {
    "table_id": "t_001",
    "request_id": "req_43",
    "row": 12,
    "column": "plot",
    "mime": "image/png",
    "data": "<base64>"
  }
}
```

### 2.4 Messages: Viewer → REPL

#### `get_data`

Request a page of formatted data.

```json
{
  "method": "get_data",
  "params": {
    "table_id": "t_001",
    "request_id": "req_42",
    "row_offset": 5000,
    "num_rows": 256
  }
}
```

#### `get_profile`

Request summary statistics for a column. This is a Positron feature worth copying.

```json
{
  "method": "get_profile",
  "params": {
    "table_id": "t_001",
    "request_id": "req_44",
    "column": "dep_time",
    "profile_type": "histogram",
    "params": { "num_bins": 20 }
  }
}
```

Supported `profile_type` values: `summary_stats`, `histogram`, `frequency_table`. The REPL computes these and returns formatted results. This is a phase 2 feature.

#### `edit_cell`

Request to modify a single cell value. The viewer sends the new value as a string; the REPL parses it according to the column's R type and performs the assignment.

```json
{
  "method": "edit_cell",
  "params": {
    "table_id": "t_001",
    "request_id": "req_45",
    "row": 1042,
    "column": "carrier",
    "value": "DL"
  }
}
```

Setting `value` to `null` writes `NA`. The REPL responds with an `edit_response`:

```json
{
  "method": "edit_response",
  "params": {
    "table_id": "t_001",
    "request_id": "req_45",
    "success": true,
    "formatted_value": "DL",
    "secondary_value": null,
    "error": null
  }
}
```

On success, the viewer updates the single cell in place (no need to refetch the page). On failure, `success` is false and `error` contains a human-readable message (e.g., `"cannot assign character to integer column"`, `"column 'id' is read-only"`).

**Edit semantics:**

- The edit modifies the actual R object in the REPL session. This is a real mutation, not ephemeral. The equivalent R code would be `df$carrier[1042] <- "DL"`.
- The REPL may optionally echo the equivalent R expression to the console, so the user has a record of the change in their session history.
- For `labelled` columns, the viewer sends the underlying code (from `secondary`), not the label text. The REPL validates against the label set.
- For `factor` columns, the value must be an existing level. The REPL returns an error if the value is not in the factor's levels (or the REPL can add the level — this is a policy decision).
- For read-only data sources (e.g., Arrow IPC files opened from disk, database connections), `edit_cell` returns `success: false` with an appropriate error. The viewer should detect read-only tables from the schema and disable editing UI.

**Schema extension for editability:**

```json
{
  "method": "open_table",
  "params": {
    "table_id": "t_001",
    "title": "flights",
    "editable": true,
    "schema": { "..." }
  }
}
```

The `editable` flag tells the viewer whether to enable cell editing UI. Arrow files and other non-mutable sources set this to `false`.

#### `close_table`

The user closed the viewer tab.

```json
{
  "method": "close_table",
  "params": { "table_id": "t_001" }
}
```

The REPL can unpin / `R_ReleaseObject` the associated SEXP.

### 2.5 Backend Architecture

Two backends, same output format (2D array of display strings + column metadata):

```
┌────────────────────────────────┐
│         OCaml REPL             │
│                                │
│  view(x) dispatches to:       │
│                                │
│  ┌──────────┐  ┌────────────┐ │
│  │  Arrow    │  │  R SEXP    │ │
│  │  backend  │  │  backend   │ │
│  │  (C/Rust) │  │  (C)       │ │
│  └─────┬────┘  └─────┬──────┘ │
│        │              │        │
│        ▼              ▼        │
│   ┌─────────────────────┐     │
│   │  Formatted page     │     │
│   │  (JSON strings)     │     │
│   └──────────┬──────────┘     │
│              │                 │
│         Unix socket            │
└──────────────┼─────────────────┘
               ▼
         Viewer process
```

**Arrow backend:** For Arrow IPC files and in-memory Arrow arrays. Uses Arrow C Data Interface. Column slicing is a pointer offset, string formatting is straightforward. This is the fast path.

**R SEXP backend (C layer):** For native R vectors, data.frames, data.tables. The C code:
  - Takes a SEXP (the data frame), row range, and optional sort index
  - For each column, switches on `TYPEOF`:
    - `REALSXP` → `snprintf` each double, check `ISNA` for NA
    - `INTSXP` → `snprintf` each int, check `== NA_INTEGER` for NA
    - `LGLSXP` → "TRUE" / "FALSE" / NA
    - `STRSXP` → `CHAR(STRING_ELT(col, i))`, check `== NA_STRING`
    - Factor → integer lookup into levels character vector
    - Date/POSIXct → format the numeric value as date string
    - `haven_labelled` → lookup value in label set, return label as primary, code as secondary
    - List columns → brief type summary like `<dbl [5]>` or `<df [3×2]>`
    - Everything else → call R's `format()` and capture the string output
  - Returns the page as a C struct that OCaml marshals to JSON

The `format()` fallback for exotic types is wrapped in `R_tryCatch` with a timeout to avoid hanging on pathological `format` methods.

**ALTREP:** Calling `REAL()` / `INTEGER()` on ALTREP vectors forces materialization. For known ALTREP types (compact sequences, deferred strings), the backend can produce display strings without materializing. This is an optimization for phase 2.

### 2.6 Viewer Page Cache

The viewer maintains a small LRU cache of pages (e.g., 10 pages of 256 rows = 2560 rows of strings in memory). On scroll, it checks whether the needed page is cached; if not, it sends a `get_data` request. Pages are evicted when distant from the current scroll position.

Cache is invalidated entirely on `update_table` (which fires after edits or external modifications to the R object).

### 2.7 Object Lifecycle

When `view(df)` is called:
1. The REPL calls `R_PreserveObject(sexp)` to prevent GC
2. Sends `open_table` with schema to the viewer
3. The viewer requests pages as needed

When the viewer tab is closed:
1. Viewer sends `close_table`
2. The REPL calls `R_ReleaseObject(sexp)`

When the REPL session ends:
1. The REPL closes the socket
2. The viewer detects disconnection and enters offline mode (cached data remains visible but no new pages can be fetched)


## 3. Things to Copy from Positron

### 3.1 Definitely Copy

These are battle-tested design decisions from Positron's data explorer that directly apply.

**Column display type taxonomy.** Positron distinguishes: `number` (real), `integer`, `string`, `boolean`, `date`, `datetime`. They added integer vs. floating distinction specifically because histogram binning and formatting differ (integers get whole-number bin edges). Copy this taxonomy, add `factor` and `blob` for our purposes.
- *Source:* `posit-dev/positron` issue #2614, Ark's `ColumnDisplayType` enum

**Format everything to strings on the backend.** The viewer never parses numeric values, never interprets dates, never handles locale-specific formatting. The backend does all formatting. This is the single most important architectural decision Positron made and it radically simplifies the viewer.
- *Source:* Ark's `harp` crate, `formatted_vector.rs`

**Column labels rendered in cells.** R supports a `label` attribute on vectors (common in survey data, Stata imports, clinical data). Positron shows these as tooltips on column headers. We go further: for labelled vectors (as produced by the `haven` package, common in clinical/survey data), the value label should be the primary display value in the cell, with the underlying code shown as secondary text or on hover. For example, a labelled integer where `1 = "Strongly Agree"` should show "Strongly Agree" in the cell, not "1". Column-level labels (the column's own label attribute) should appear in the column header subtitle, below the column name.
- *Source:* `posit-dev/positron` issue #2971, extended for labelled data

**Summary panel with histograms and frequency tables.** This is the killer UX feature of Positron's data explorer. Each column gets a small histogram (numeric) or frequency bar chart (categorical) plus summary stats (mean, median, min, max, NA count, unique count). Users rave about this.
- *Source:* Positron summary panel docs, Wes McKinney's posit::conf talk

**Copy-paste as tab-separated values.** Select a rectangular region, Ctrl+C, paste into Excel/Sheets. Column headers included. This is table-stakes functionality that Positron gets right.
- *Source:* Positron data grid docs

**Ephemeral view philosophy.** Positron explicitly frames the data explorer as complementing, not replacing, code-based workflows. The viewer watches for changes to the underlying object. This framing should be copied in documentation and UX — with the addition that our viewer supports spot edits, which are real mutations echoed to the console.
- *Source:* Positron data explorer overview

**List column cell summaries.** For list columns in data frames, show a compact type summary in each cell: `<dbl [5]>`, `<chr [3]>`, `<df [10×4]>`, `NULL`. This is what tibble's pillar package does and Positron adopts it.
- *Source:* Ark's data explorer, tibble conventions

### 3.2 Copy with Modifications

**Column pinning (simplified).** Positron supports pinning columns to the left and rows to the top. Column pinning is very useful for datasets with an ID column + many feature columns. Row pinning is niche. Copy column pinning, skip row pinning initially.
- *Source:* Positron 2025.10.0 release notes

**Live object watching (simplified).** Positron watches the R environment for changes to viewed objects and auto-refreshes. In our architecture, the REPL knows when the user modifies the object (because the REPL evaluates the code), so the REPL can push `update_table` proactively. This is simpler than Positron's polling/hashing approach. Edits made via the viewer also trigger `update_table` to keep summary stats current.

**Summary panel (without filter integration).** Positron's summary panel has deep integration with the filter bar — double-clicking NA counts creates a filter, etc. Since we don't have filters, the summary panel is purely informational: histograms, frequency tables, summary stats, NA counts. Still very valuable for data exploration.
- *Source:* Positron summary panel docs

### 3.3 Study But Don't Copy

**Jupyter comm channel transport.** Positron uses Jupyter's custom comm messages over ZeroMQ. This is right for their architecture (they need to work within VS Code's extension model and support multiple language kernels). For a standalone viewer connected by unix socket, this is unnecessary overhead.

**Custom virtual grid renderer.** Positron built a custom React-based virtual data grid from scratch (they evaluated TanStack Table and decided to roll their own for performance). For the Swift viewer, use NSTableView which already has row virtualization built in. For GTK4, use GtkColumnView. Don't build a custom grid unless the native widget is inadequate.

**Comm-based architecture.** Positron's Ark kernel has a `CommManager` abstraction for managing bidirectional communication channels, with typed protocols and lifecycle management. This is good engineering for a large IDE project. For a standalone viewer, a simple message dispatch loop over a unix socket is sufficient.

**RThreadSafe\<T\> wrapper.** Ark wraps R objects in a `RThreadSafe<T>` struct that enforces main-thread access and handles cross-thread Drop via `spawn_interrupt()`. This is needed because Ark is multi-threaded (LSP, DAP, kernel all on different threads). Our REPL likely has a simpler threading model — if the OCaml REPL is single-threaded with R, a simple `R_PreserveObject` / `R_ReleaseObject` pair is sufficient.

**DuckDB integration for file-based viewing.** Positron uses DuckDB to open CSV/Parquet files directly in the data explorer without loading into R/Python. This is clever for an IDE but out of scope for a REPL data viewer. If a user wants to view a file, they load it into R first.


### 3.4 Key Files to Read in the Positron/Ark Repos

| What | Where | Why |
|------|-------|-----|
| R vector → display string | `posit-dev/ark` `crates/harp/src/vector/formatted_vector.rs` | The Rust equivalent of our C formatting layer. Pattern-matches on SEXPTYPE, handles NA, factors, dates, POSIXct, list columns. |
| Data explorer backend | `posit-dev/ark` `crates/ark/src/data_explorer/` | How Ark handles schema introspection, data page requests, sorting, filtering, and column profiling for R objects. |
| Format helpers (R side) | `posit-dev/ark` `crates/harp/src/modules/format.R` | R helper functions called from Rust for formatting exotic types. Shows which types need R-side `format()` vs. Rust-native formatting. |
| Data explorer frontend | `posit-dev/positron` `src/vs/workbench/services/positronDataExplorer/` | The TypeScript/React frontend. Most of this is VS Code-specific, but the grid virtualization logic and cell rendering are instructive. |
| Column display types | `posit-dev/positron` issue #2614 | Discussion of integer vs. decimal display type distinction and its impact on histogram binning. |
| Convert to code | `posit-dev/positron` issue #9030 | How they generate tidyverse code from data explorer state. |
