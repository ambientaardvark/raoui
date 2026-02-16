(* language_syntax.mli *)

(** Generic incremental lexer cache and language-specific syntax analysis.

    This module provides:
    - A functor for building incremental lexer caches (language-agnostic)
    - Common types for syntax highlighting and continuation detection
*)

(** {1 Language-agnostic lexer interface} *)

(** Signature that any language lexer must implement to use the cache *)
module type LEXER = sig
  (** Token type produced by the lexer *)
  type token

  (** Lexer mode/state (e.g., inside string, inside comment) *)
  type mode

  (** Initial lexer mode at start of file *)
  val initial_mode : mode

  (** Lex a single line, given starting mode.
      Returns tokens and the mode at end of line. *)
  val lex_line : mode -> string -> token list * mode
end

(** {1 Incremental lexer cache} *)

(** Functor for building a cached incremental lexer *)
module Make (L : LEXER) : sig
  (** A cached lexer entry for one line *)
  type entry = {
    text : string;              (** Original line text *)
    tokens : L.token list;      (** Tokens produced *)
    start_mode : L.mode;        (** Lexer mode at start of line *)
    end_mode : L.mode;          (** Lexer mode at end of line *)
  }

  (** The cache: one entry per line *)
  type t = entry list

  (** {2 Construction} *)

  (** Create a fresh cache for the given lines *)
  val create : Unicode_string.t list -> t

  (** {2 Incremental updates} *)

  (** Update cache when lines change.

      [update ~start_line ~end_line ~lines cache] efficiently recomputes
      the cache when lines [start_line] through [end_line] have changed.

      - Lines before [start_line] are assumed unchanged and reused
      - Lines [start_line..end_line] are re-lexed
      - Lines after [end_line] may be re-lexed if their start_mode changed

      Falls back to full re-lex if cache structure is inconsistent.
  *)
  val update :
    start_line:int ->
    end_line:int ->
    lines:Unicode_string.t list ->
    t ->
    t

  (** {2 Queries} *)

  (** Get all tokens from the beginning up to (not including) [line] *)
  val tokens_before_line : t -> line:int -> L.token list

  (** Get all tokens up to and including [line] *)
  val tokens_through_line : t -> line:int -> L.token list

  (** Get the entry for a specific line *)
  val get_entry : t -> line:int -> entry option

  (** Get tokens for a specific line *)
  val get_line_tokens : t -> line:int -> L.token list option

  (** Get the lexer mode at the start of a given line *)
  val get_start_mode : t -> line:int -> L.mode option
end

(** {1 Language-specific continuation detection} *)

(** Signature for language-specific "is this statement complete?" logic *)
module type CONTINUATION = sig
  (** Token type (typically matches the lexer's token type) *)
  type token

  (** Result of continuation analysis *)
  type signal =
    | Submit  (** Statement is complete, should submit to REPL *)
    | Continue of {
        indent_levels : int;     (** How many levels to indent continuation *)
        in_empty_brackets : bool (** Special case: cursor is in {} and should expand *)
      }

  (** Analyze tokens to determine if more input is needed.

      Typically checks for:
      - Unclosed delimiters (parens, brackets, braces)
      - Trailing operators or keywords that require continuation
      - Language-specific incomplete constructs
  *)
  val analyze : token list -> signal

  (** Check if cursor is positioned inside empty brackets (for special Enter behavior) *)
  val inside_empty_brackets :
    tokens:token list ->
    cursor_byte_offset:int ->
    bool
end

(** {1 Byte offset utilities} *)

(** Calculate the byte offset of a cursor position within a line.
    Needed for token-based analysis since tokens use byte positions. *)
val cursor_byte_offset :
  line:Unicode_string.t ->
  cursor_pos:int ->
  int

(** Find tokens around a specific byte position (for context-aware features) *)
val tokens_with_positions :
  'token list ->
  token_to_lexeme:('token -> string) ->
  (('token * int * int) list)
  (** Returns list of [(token, start_byte, end_byte)] *)
