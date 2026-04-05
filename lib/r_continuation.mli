(** R-specific continuation analysis used by Enter handling. *)

type signal =
  | Submit
  | Continue of {
      indent_levels : int;
      in_empty_brackets : bool;
    }

val analyze : R_lexer.token list -> signal

val inside_empty_brackets :
  tokens:R_lexer.token list ->
  cursor_byte_offset:int ->
  bool
