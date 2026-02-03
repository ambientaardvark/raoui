(** Syntax highlighting and expression continuation checking for R code *)

(** Convert a token to its lexeme string *)
val token_to_lexeme : R_lexer.token -> string

(** Convert a token to a styled span *)
val token_to_span : R_lexer.token -> Terminal_ops.span

(** Convert a token list to spans, with lookahead for function detection *)
val tokens_to_spans : R_lexer.token list -> Terminal_ops.span list

(** Parse {expr} interpolation starting after the {. Returns spans and position after }. *)
val parse_interpolation : string -> int -> Terminal_ops.span list * int

(** Parse a glue string (with quotes) for {expr} interpolations *)
val parse_glue_string : string -> Terminal_ops.span list

(** Highlight a line of R code, returning styled spans and new mode *)
val highlight_line :
  R_lexer.mode -> string -> Terminal_ops.span list * R_lexer.mode

(* Continuation logic moved to Update; syntax only handles highlighting. *)
