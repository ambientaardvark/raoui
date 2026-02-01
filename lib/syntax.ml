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
  | LAMBDA -> `Function
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
  | LAMBDA -> "\\"
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

(** Convert a token list to spans, with lookahead for function detection *)
let tokens_to_spans (tokens : Lexer.token list) : span list =
  let rec loop acc = function
    | [] -> List.rev acc
    | hd :: next :: tl -> (
        match (hd, next) with
        | Lexer.IDENT func_name, Lexer.LEFT_PAREN ->
            loop (token_to_span next :: (`Function, func_name) :: acc) tl
        | _ -> loop (token_to_span hd :: acc) (next :: tl))
    | hd :: tl -> loop (token_to_span hd :: acc) tl
  in
  loop [] tokens

(** Highlight a line of R code, returning styled spans *)
let highlight_line (mode : Lexer.mode) (line : string) : span list * Lexer.mode
    =
  let tokens, mode_out = Lexer.lex_line mode line in
  let rec loop acc (ts : Lexer.token list) =
    match ts with
    | [] -> List.rev acc
    | hd :: next :: tl -> (
        match (hd, next) with
        | IDENT func_name, LEFT_PAREN ->
            loop (token_to_span next :: (`Function, func_name) :: acc) tl
        | _ -> loop (token_to_span hd :: acc) (next :: tl))
    | hd :: tl -> loop (token_to_span hd :: acc) tl
  in
  let spans = loop [] tokens in
  (spans, mode_out)
