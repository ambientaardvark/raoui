type token =
  | NUMBER of string
  | STRING of string
  | COMMENT of string
  | KEYWORD of string
  | CONSTANT of string
  | OPERATOR of string
  | IDENT of string
  | BACKTICK_IDENT of string
  | PUNCTUATION of string
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | LEFT_BRACE
  | RIGHT_BRACE
  | WHITESPACE of string
  | UNKNOWN of string
  | EOF

type mode =
  | Normal
  | In_double_quote
  | In_single_quote
  | In_backtick
  | In_raw_string

let digit = [%sedlex.regexp? '0' .. '9']

let _fancy_number =
  [%sedlex.regexp? Opt ('0', Chars "xb"), digit, Star (digit | 'e' | 'i' | '.')]

let number = [%sedlex.regexp? Plus digit, Opt (".", Plus digit)]
let double_quote_string = [%sedlex.regexp? '"', Star (Compl '"' | "\\\""), '"']
let _raw_string_start = [%sedlex.regexp? "r\""]
let whitespace = [%sedlex.regexp? Plus (Chars "\t ")]
let punctuation = [%sedlex.regexp? Chars ","]
let wildcard_operator = [%sedlex.regexp? '%', Plus (Compl '%'), '%']
let escaped_char = [%sedlex.regexp? "\\", any]

let multi_char_operators =
  [%sedlex.regexp?
    ( ">=" | "<=" | "<-" | "->" | "<<-" | "->>" | ":::" | "::" | "|>" | "=="
    | "!=" | "%%" | "||" )]

let operators = [%sedlex.regexp? Chars "+-*/%^&|=<>!"]

let read_double_quote buf =
  let open Sedlexing in
  let rec loop acc buf =
    match%sedlex buf with
    | escaped_char -> loop (Utf8.lexeme buf :: acc) buf
    | '"' -> (STRING ((acc |> List.rev |> String.concat "") ^ "\""), Normal)
    | eof ->
        if List.length acc = 0 then (EOF, In_double_quote)
        else (STRING (acc |> List.rev |> String.concat ""), In_double_quote)
    | any -> loop (Utf8.lexeme buf :: acc) buf
    | _ -> failwith "no match"
  in
  loop [] buf

let read_normal buf =
  let open Sedlexing in
  match%sedlex buf with
  | '"' -> (STRING "\"", In_double_quote)
  | number -> (NUMBER (Utf8.lexeme buf), Normal)
  | '(' -> (LEFT_PAREN, Normal)
  | ')' -> (RIGHT_PAREN, Normal)
  | '[' -> (LEFT_BRACKET, Normal)
  | ']' -> (RIGHT_BRACKET, Normal)
  | '{' -> (LEFT_BRACE, Normal)
  | '}' -> (RIGHT_BRACE, Normal)
  | wildcard_operator -> (OPERATOR (Utf8.lexeme buf), Normal)
  | multi_char_operators -> (OPERATOR (Utf8.lexeme buf), Normal)
  | operators -> (OPERATOR (Utf8.lexeme buf), Normal)
  | punctuation -> (PUNCTUATION (Utf8.lexeme buf), Normal)
  | whitespace -> (WHITESPACE (Utf8.lexeme buf), Normal)
  | eof -> (EOF, Normal)
  | any -> (UNKNOWN (Utf8.lexeme buf), Normal)
  | _ -> failwith "no match"

let token mode buf =
  match mode with
  | Normal -> read_normal buf
  | In_double_quote -> read_double_quote buf
  | _ -> failwith "not implemented"

let print_type = function
  | NUMBER s -> Printf.sprintf "NUMBER: %s" s
  | STRING s -> Printf.sprintf "STRING: %s`" s
  | COMMENT s -> Printf.sprintf "COMMENT: %s" s
  | KEYWORD s -> Printf.sprintf "KEYWORD: %s" s
  | CONSTANT s -> Printf.sprintf "CONSTANT: %s" s
  | OPERATOR s -> Printf.sprintf "OPERATOR: %s" s
  | IDENT s -> Printf.sprintf "IDENT: %s" s
  | BACKTICK_IDENT s -> Printf.sprintf "BACKTICK_IDENT: %s" s
  | PUNCTUATION s -> Printf.sprintf "PUNCTUATION: %s" s
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACKET -> "LEFT_BRACKET"
  | RIGHT_BRACKET -> "RIGHT_BRACKET"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | WHITESPACE s -> Printf.sprintf "WHITESPACE: %s" s
  | UNKNOWN s -> Printf.sprintf "UNKNOWN: %s" s
  | EOF -> "EOF"

let lex_line mode line =
  let for_lexer = Sedlexing.Utf8.from_string line in
  let rec loop acc mode buf =
    if List.length acc > 10 then
      failwith
        (Printf.sprintf "too much iteration: %s"
           (acc |> List.map print_type |> String.concat ", "))
    else
      match token mode buf with
      | EOF, m -> (List.rev acc, m)
      | t, m -> loop (t :: acc) m buf
  in
  loop [] mode for_lexer
