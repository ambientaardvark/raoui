(** Incremental lexer cache for R code. *)

type entry = {
  text : string;
  tokens : R_lexer.token list;
  start_mode : R_lexer.mode;
  end_mode : R_lexer.mode;
}

type t = entry list

val create : Unicode_string.t list -> t
val make_all_default : Unicode_string.t list -> t

val update :
  start_line:int ->
  end_line:int ->
  lines:Unicode_string.t list ->
  t ->
  t

val tokens_before_line : t -> line:int -> R_lexer.token list
val tokens_through_line : t -> line:int -> R_lexer.token list
val get_entry : t -> line:int -> entry option
val get_line_tokens : t -> line:int -> R_lexer.token list option
val get_start_mode : t -> line:int -> R_lexer.mode option
