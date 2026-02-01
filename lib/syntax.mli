(** Syntax highlighting and expression continuation checking for R code *)

(** Convert a token to its lexeme string *)
val token_to_lexeme : R_lexer.token -> string

(** Convert a token to a styled span *)
val token_to_span : R_lexer.token -> Terminal_ops.span

(** Highlight a line of R code, returning styled spans and new mode *)
val highlight_line :
  R_lexer.mode -> string -> Terminal_ops.span list * R_lexer.mode

(** Accumulated state across lines for continuation checking *)
type parse_state = {
  mode : R_lexer.mode;
  parens : int;    (** unclosed ( count *)
  brackets : int;  (** unclosed [ count *)
  braces : int;    (** unclosed { count *)
}

val initial_state : parse_state

(** Process a line, updating accumulated state and returning styled spans *)
val update_state : parse_state -> string -> parse_state * Terminal_ops.span list

(** Result of checking if an expression needs continuation *)
type continuation_info = {
  needs_continuation : bool;
  indent_delta : int;  (** Suggested indent change: +1 for open, -1 for close *)
}

(** Check if expression needs continuation.
    Call this with the accumulated state from earlier lines and the LAST line's content.
    Only the last line is checked for trailing operators/keywords. *)
val check_continuation : parse_state -> string -> continuation_info

(** Convenience: process multiple lines and check continuation *)
val check_lines : string list -> continuation_info
