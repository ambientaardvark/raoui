open Base

(** Terminal operations abstraction layer.
    Provides a typed interface for terminal control that can be rendered
    to different terminal types (ANSI, etc.) *)

(** Style for terminal output *)
type style =
  [ `Raw
  | `Plain
  | `Accent
  | `Error
  | `Keyword
  | `String
  | `Number
  | `Comment
  | `Operator
  | `Constant
  | `Ident
  | `Bracket
  ]

(** A styled span of text *)
type span = style * string

(** Styled text - list of spans *)
type term_output = span list

type direction =
  | Left of int
  | Right of int
  | Up of int
  | Down of int

type op =
  | Print of term_output
  | Print_char of char
  | Newline
  | Cursor_to of int * int       (** absolute position - row, col - 1-indexed *)
  | Cursor_shift of direction    (** relative movement *)
  | Clear_to_eol
  | Scroll_up of int
  | Scroll_down of int
  | Show_cursor
  | Hide_cursor

module type TERMINAL = sig
  val render : op Queue.t -> string
end

module type CONFIG = sig
  val term_type : string
end

module Make (_ : CONFIG) : TERMINAL

(** Render spans to ANSI string *)
val render_spans : span list -> string
