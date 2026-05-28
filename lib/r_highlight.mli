(** R syntax highlighting and token rendering. *)

open Terminal_ops

type styled_range = { start_byte : int; end_byte : int; style : style }

val style_of_token : R_lexer.token -> style
val token_to_lexeme : R_lexer.token -> string
val token_to_span : R_lexer.token -> span
val parse_interpolation : string -> int -> span list * int
val parse_glue_string : string -> span list
val tokens_to_spans : R_lexer.token list -> span list
val ranges_for_entry : R_lex_cache.entry -> styled_range list
val highlight_line : R_lexer.mode -> string -> span list * R_lexer.mode
