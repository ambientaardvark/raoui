(** Markdown -> wrapped terminal layout.

    Pure: depends on neither [Terminal_ops], [Theme], nor [R_highlight]. Parses
    CommonMark (via cmarkit) and lays prose out to a fixed column width; fenced
    code blocks are passed through raw for the host to render and wrap. This is
    the package-shaped core — all markdown semantics live here and are
    golden-testable without a terminal. *)

(* Emphasis attributes. They STACK on top of a role, so combinations such as
   bold-inside-heading or italic-inside-quote survive (a flat enum could not
   express them). *)
type emphasis = {
  italic : bool;   (* from * or _ *)
  bold : bool;     (* from ** or __ *)
  strike : bool;   (* from ~~...~~ (a cmarkit non-strict extension) *)
}

(* The empty-emphasis value. *)
val no_emphasis : emphasis

(* The semantic role of a run of text; selects the base face downstream. *)
type role =
  | Body            (* ordinary paragraph text *)
  | Heading of int  (* ATX/setext heading, level 1..6 *)
  | Quote           (* text inside a blockquote *)
  | Code_span       (* inline `code` *)
  | Link of string  (* visible link text; carries the destination URL *)
  | Marker          (* list bullets, ordered numbers, quote bars, rules *)

(* Full styling of one text run. *)
type style = { role : role; emphasis : emphasis }

(* A styled run of plain text; never contains a newline. *)
type span = style * string

(* One already-wrapped visual line. Text widths sum to <= the render width.
   Continuation lines carry their hanging-indent prefix as leading spans, so
   list and quote wrapping stays aligned. An empty list is a blank line. *)
type line = span list

(* A fenced code block md_layout deliberately does not style. [info] is the
   trimmed fence info string (e.g. "r"), [code] the raw block contents, and
   [indent] the left margin in columns for blocks nested in lists/quotes. *)
type code_block = { info : string; code : string; indent : int }

(* md_layout's output, in document order. Prose is pre-wrapped; code is raw. *)
type block =
  | Lines of line list
  | Code of code_block

(* [render ~width ~measure md] parses a COMPLETE CommonMark document and lays
   it out to [width] columns. [measure] returns the terminal display width of a
   string — the host passes a Unicode-aware function; tests may pass
   [String.length]. *)
val render : width:int -> measure:(string -> int) -> string -> block list
