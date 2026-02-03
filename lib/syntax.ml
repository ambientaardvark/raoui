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

(** Parse {expr} interpolation starting after the {. Returns spans and position after }. *)
let parse_interpolation (s : string) (start : int) : span list * int =
  let len = String.length s in
  let rec find_close depth p =
    if p >= len then None
    else
      match s.[p] with
      | '{' -> find_close (depth + 1) (p + 1)
      | '}' when depth = 1 -> Some p
      | '}' -> find_close (depth - 1) (p + 1)
      | _ -> find_close depth (p + 1)
  in
  match find_close 1 start with
  | None ->
      (* No matching }, treat { and rest as string *)
      ([ (`String, "{" ^ String.sub s start (len - start)) ], len)
  | Some close_pos ->
      let expr = String.sub s start (close_pos - start) in
      let expr_tokens, _ = Lexer.lex_line Lexer.Normal expr in
      let expr_spans = List.map token_to_span expr_tokens in
      ([ (`Bracket, "{") ] @ expr_spans @ [ (`Bracket, "}") ], close_pos + 1)

let parse_glue_string (s : string) : span list =
  let len = String.length s in
  if len < 2 || s.[0] <> '"' then [ (`String, s) ]
  else
    let has_close = s.[len - 1] = '"' in
    let inner =
      if has_close then String.sub s 1 (len - 2) else String.sub s 1 (len - 1)
    in
    let inner_len = String.length inner in
    let rec parse acc pos =
      if pos >= inner_len then List.rev acc
      else
        match inner.[pos] with
        | '{' when pos + 1 < inner_len && inner.[pos + 1] = '{' ->
            parse ((`String, "{{") :: acc) (pos + 2)
        | '{' ->
            let spans, next_pos = parse_interpolation inner (pos + 1) in
            parse (List.rev_append spans acc) next_pos
        | '}' when pos + 1 < inner_len && inner.[pos + 1] = '}' ->
            parse ((`String, "}}") :: acc) (pos + 2)
        | '}' -> parse ((`String, "}") :: acc) (pos + 1)
        | _ ->
            let rec find_brace p =
              if p >= inner_len then p
              else match inner.[p] with '{' | '}' -> p | _ -> find_brace (p + 1)
            in
            let end_pos = find_brace pos in
            parse ((`String, String.sub inner pos (end_pos - pos)) :: acc) end_pos
    in
    let inner_spans = parse [] 0 in
    (`String, "\"")
    :: inner_spans
    @ if has_close then [ (`String, "\"") ] else []

(** Convert a token list to spans, with lookahead for function detection *)
let tokens_to_spans (tokens : Lexer.token list) : span list =
  let rec loop acc = function
    | [] -> List.rev acc
    | Lexer.IDENT "glue" :: Lexer.LEFT_PAREN :: Lexer.STRING s :: tl ->
        let glue_spans = parse_glue_string s in
        loop
          (List.rev_append glue_spans
             ((`Bracket, "(") :: (`Function, "glue") :: acc))
          tl
    | Lexer.IDENT func_name :: Lexer.LEFT_PAREN :: tl ->
        loop ((`Bracket, "(") :: (`Function, func_name) :: acc) tl
    | hd :: tl -> loop (token_to_span hd :: acc) tl
  in
  loop [] tokens

(** Highlight a line of R code, returning styled spans *)
let highlight_line (mode : Lexer.mode) (line : string) : span list * Lexer.mode
    =
  let tokens, mode_out = Lexer.lex_line mode line in
  (tokens_to_spans tokens, mode_out)
