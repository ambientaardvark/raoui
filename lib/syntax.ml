open Base
open Terminal_ops
module Lexer = R_lexer

(** Convert a token to a styled span *)
let style_of_token : Lexer.token -> style = function
  | NUMBER _ -> `Number
  | STRING _ -> `String
  | COMMENT _ -> `Comment
  | KEYWORD _ -> `Keyword
  | CONSTANT _ -> `Constant
  | OPERATOR _ -> `Operator
  | IDENT _ -> `Ident
  | BACKTICK_IDENT _ -> `Ident
  | PUNCTUATION _ -> `Plain
  | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACKET | RIGHT_BRACKET | LEFT_BRACE
  | RIGHT_BRACE ->
      `Bracket
  | WHITESPACE _ -> `Plain
  | UNKNOWN _ -> `Error
  | EOF -> `Plain

let token_to_lexeme : Lexer.token -> string = function
  | NUMBER s -> s
  | STRING s -> s
  | COMMENT s -> s
  | KEYWORD kw -> Lexer.string_of_keyword kw
  | CONSTANT s -> s
  | OPERATOR s -> s
  | IDENT s -> s
  | BACKTICK_IDENT s -> s
  | PUNCTUATION s -> s
  | LEFT_PAREN -> "("
  | RIGHT_PAREN -> ")"
  | LEFT_BRACKET -> "["
  | RIGHT_BRACKET -> "]"
  | LEFT_BRACE -> "{"
  | RIGHT_BRACE -> "}"
  | WHITESPACE s -> s
  | UNKNOWN s -> s
  | EOF -> ""

(** Convert a token to a styled span *)
let token_to_span (token : Lexer.token) : span =
  (style_of_token token, token_to_lexeme token)

(** Highlight a line of R code, returning styled spans *)
let highlight_line (mode : Lexer.mode) (line : string) : span list * Lexer.mode =
  let tokens, mode_out = Lexer.lex_line mode line in
  let spans = List.map tokens ~f:token_to_span in
  (spans, mode_out)

(** Accumulated state across lines for continuation checking *)
type parse_state = {
  mode : Lexer.mode;
  parens : int;    (** unclosed ( count *)
  brackets : int;  (** unclosed [ count *)
  braces : int;    (** unclosed { count *)
}

let initial_state = { mode = Lexer.Normal; parens = 0; brackets = 0; braces = 0 }

(** Update bracket counts from a token *)
let update_brackets state token =
  match token with
  | Lexer.LEFT_PAREN -> { state with parens = state.parens + 1 }
  | Lexer.RIGHT_PAREN -> { state with parens = max 0 (state.parens - 1) }
  | Lexer.LEFT_BRACKET -> { state with brackets = state.brackets + 1 }
  | Lexer.RIGHT_BRACKET -> { state with brackets = max 0 (state.brackets - 1) }
  | Lexer.LEFT_BRACE -> { state with braces = state.braces + 1 }
  | Lexer.RIGHT_BRACE -> { state with braces = max 0 (state.braces - 1) }
  | _ -> state

(** Check if any brackets are unclosed *)
let has_unclosed state =
  state.parens > 0 || state.brackets > 0 || state.braces > 0

(** Process a line, updating accumulated state *)
let update_state (state : parse_state) (line : string) : parse_state * span list =
  let tokens, mode_out = Lexer.lex_line state.mode line in
  let spans = List.map tokens ~f:token_to_span in
  let state' = List.fold tokens ~init:{ state with mode = mode_out } ~f:update_brackets in
  (state', spans)

(** Check if the last non-whitespace token expects continuation *)
let ends_with_continuation tokens =
  let rec first_non_ws = function
    | [] -> None
    | Lexer.WHITESPACE _ :: rest -> first_non_ws rest
    | x :: _ -> Some x
  in
  match first_non_ws (List.rev tokens) with
  | None -> false
  | Some token -> (
      match token with
      (* Keywords that expect something after *)
      | Lexer.KEYWORD Lexer.IF -> true
      | Lexer.KEYWORD Lexer.ELSE -> true
      | Lexer.KEYWORD Lexer.FOR -> true
      | Lexer.KEYWORD Lexer.WHILE -> true
      | Lexer.KEYWORD Lexer.FUNCTION -> true
      | Lexer.KEYWORD Lexer.REPEAT -> true
      | Lexer.KEYWORD Lexer.IN -> true
      (* Operators that expect something after *)
      | Lexer.OPERATOR _ -> true
      (* Open brackets *)
      | Lexer.LEFT_PAREN | Lexer.LEFT_BRACKET | Lexer.LEFT_BRACE -> true
      (* Comma in argument list *)
      | Lexer.PUNCTUATION "," -> true
      | _ -> false)

(** Result of checking if an expression needs continuation *)
type continuation_info = {
  needs_continuation : bool;
  indent_delta : int;  (** Suggested indent change: +1 for open, -1 for close *)
}

(** Check if expression needs continuation.
    Call this with the accumulated state and the LAST line's content. *)
let check_continuation (state : parse_state) (last_line : string) : continuation_info =
  let tokens, mode_out = Lexer.lex_line state.mode last_line in
  let state' = List.fold tokens ~init:{ state with mode = mode_out } ~f:update_brackets in

  (* Check conditions *)
  let in_multiline = match state'.mode with Lexer.Normal -> false | _ -> true in
  let unclosed = has_unclosed state' in
  let trailing = ends_with_continuation tokens in

  let needs_continuation = in_multiline || unclosed || trailing in

  (* Indent delta based on this line's net bracket change *)
  let line_balance =
    List.fold tokens ~init:0 ~f:(fun acc token ->
      match token with
      | Lexer.LEFT_PAREN | Lexer.LEFT_BRACKET | Lexer.LEFT_BRACE -> acc + 1
      | Lexer.RIGHT_PAREN | Lexer.RIGHT_BRACKET | Lexer.RIGHT_BRACE -> acc - 1
      | _ -> acc)
  in
  let indent_delta = if line_balance > 0 then 1 else if line_balance < 0 then -1 else 0 in

  { needs_continuation; indent_delta }

(** Convenience: process multiple lines and check continuation *)
let check_lines (lines : string list) : continuation_info =
  match List.rev lines with
  | [] -> { needs_continuation = false; indent_delta = 0 }
  | last :: rest ->
      let earlier = List.rev rest in
      let state = List.fold earlier ~init:initial_state ~f:(fun s line ->
        fst (update_state s line))
      in
      check_continuation state last
