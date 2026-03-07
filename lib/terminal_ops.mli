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
  | `Function
  | `Completion
  | `Completion_selected
  | `Shell_prompt
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

(** Absolute cursor positioning — returns escape string *)
val cursor_to : int -> int -> string

(** Clear from cursor to end of screen *)
val clear_to_eos : string

(** Set cursor shape to solid block *)
val solid_cursor : string

(** Enable bracketed paste mode *)
val enable_bracketed_paste : string

(** Disable bracketed paste mode *)
val disable_bracketed_paste : string

(** Request cursor position report (terminal replies with ESC[row;colR) *)
val cursor_position_request : string

(** Scroll the viewport up by n lines using natural scrolling that preserves
    the scrollback buffer.  Returns an escape string. *)
val scroll_up : term_height:int -> int -> string
