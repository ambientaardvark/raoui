(** R language syntax highlighting and analysis *)

open Terminal_ops

(** {1 Lexer cache} *)

(** Incremental lexer cache for R code *)
module Cache : sig
  (** A cached lexer entry for one line *)
  type entry = {
    text : string;
    tokens : R_lexer.token list;
    start_mode : R_lexer.mode;
    end_mode : R_lexer.mode;
  }

  (** The cache: one entry per line *)
  type t = entry list

  (** Create a fresh cache for the given lines *)
  val create : Unicode_string.t list -> t

  (** Update cache when lines change *)
  val update :
    start_line:int ->
    end_line:int ->
    lines:Unicode_string.t list ->
    t ->
    t

  (** Get all tokens from the beginning up to (not including) [line] *)
  val tokens_before_line : t -> line:int -> R_lexer.token list

  (** Get all tokens up to and including [line] *)
  val tokens_through_line : t -> line:int -> R_lexer.token list

  (** Get the entry for a specific line *)
  val get_entry : t -> line:int -> entry option

  (** Get tokens for a specific line *)
  val get_line_tokens : t -> line:int -> R_lexer.token list option

  (** Get the lexer mode at the start of a given line *)
  val get_start_mode : t -> line:int -> R_lexer.mode option
end

(** {1 Syntax highlighting} *)

(** Convert an R token to a styled span *)
val style_of_token : R_lexer.token -> style

(** Convert an R token to its lexeme (source text) *)
val token_to_lexeme : R_lexer.token -> string

(** Convert an R token to a styled span *)
val token_to_span : R_lexer.token -> span

(** Parse {expr} interpolation starting after the {. Returns spans and position after }. *)
val parse_interpolation : string -> int -> span list * int

(** Parse a glue string (with quotes) for {expr} interpolations *)
val parse_glue_string : string -> span list

(** Convert a token list to styled spans (handles function detection, glue strings) *)
val tokens_to_spans : R_lexer.token list -> span list

(** Highlight a line of R code *)
val highlight_line : R_lexer.mode -> string -> span list * R_lexer.mode

(** {1 Continuation detection} *)

(** R-specific continuation detection *)
module Continuation : sig
  type signal =
    | Submit  (** Statement is complete, should submit to REPL *)
    | Continue of {
        indent_levels : int;
        in_empty_brackets : bool;
      }

  (** Analyze tokens to determine if continuation is needed *)
  val analyze : R_lexer.token list -> signal

  (** Check if cursor is inside empty braces {} *)
  val inside_empty_brackets :
    tokens:R_lexer.token list ->
    cursor_byte_offset:int ->
    bool
end
