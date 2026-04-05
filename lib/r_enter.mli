(** R-specific Enter key behavior. *)

type action =
  | Submit
  | Insert_newline of { indent : int }
  | Expand_braces of {
      inner_indent : int;
      outer_indent : int;
    }

val action :
  lines:Unicode_string.t list ->
  cursor_line:int ->
  cursor_pos:int ->
  cache:R_lex_cache.t ->
  action
