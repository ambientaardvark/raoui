type keyword =
  | IF
  | ELSE
  | FOR
  | WHILE
  | REPEAT
  | IN
  | FUNCTION
  | BREAK
  | NEXT
  | RETURN

type token =
  | NUMBER of string
  | STRING of string
  | COMMENT of string
  | KEYWORD of keyword
  | CONSTANT of string
  | OPERATOR of string
  | IDENT of string
  | BACKTICK_IDENT of string
  | PUNCTUATION of string
  | LAMBDA
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | LEFT_BRACE
  | RIGHT_BRACE
  | WHITESPACE of string
  | UNKNOWN of string
  | DEFAULT of string
  | EOF

type mode =
  | Normal
  | In_double_quote of string
  | In_single_quote of string
  | In_backtick of string
  | In_raw_string of string * string (* closing_seq, accumulated *)

let digit = [%sedlex.regexp? '0' .. '9']
let hex_digit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']
let binary_digit = [%sedlex.regexp? '0' | '1']

let integer =
  [%sedlex.regexp?
    '0', 'x', Plus hex_digit | '0', 'b', Plus binary_digit | Plus digit]

let floating_point =
  [%sedlex.regexp? Plus digit, '.', Star digit | Star digit, '.', Plus digit]

let scientific_number =
  [%sedlex.regexp?
    (floating_point | integer), Chars "eE", Opt (Chars "+-"), Plus digit]

let real_number =
  [%sedlex.regexp? (integer | floating_point | scientific_number), Opt 'L']

let imaginary_number = [%sedlex.regexp? (integer | floating_point), 'i']

let complex_number =
  [%sedlex.regexp?
    ( real_number, Chars "+-", imaginary_number
    | imaginary_number, Chars "+-", real_number )]

let r_number = [%sedlex.regexp? real_number | complex_number | imaginary_number]
let whitespace = [%sedlex.regexp? Plus (Chars "\t ")]
let punctuation = [%sedlex.regexp? Chars ",;_\\"]
let wildcard_operator = [%sedlex.regexp? '%', Plus (Compl '%'), '%']
let escaped_char = [%sedlex.regexp? "\\", any]

let constant =
  [%sedlex.regexp?
    ( "TRUE" | "FALSE" | "NULL" | "NA" | "Inf" | "NaN" | "NA_integer_"
    | "NA_real_" | "NA_complex_" | "NA_character_" )]

let keyword_of_string = function
  | "if" -> Some IF
  | "else" -> Some ELSE
  | "for" -> Some FOR
  | "while" -> Some WHILE
  | "repeat" -> Some REPEAT
  | "in" -> Some IN
  | "function" -> Some FUNCTION
  | "break" -> Some BREAK
  | "next" -> Some NEXT
  | "return" -> Some RETURN
  | _ -> None

let ident_start = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let ident_continue =
  [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.']

let ident =
  [%sedlex.regexp?
    ( ident_start, Star ident_continue
    | '.', ident_start, Star ident_continue
    | "..." )]

let uchar_of_lexeme s =
  Uutf.String.fold_utf_8
    (fun acc _ -> function
      | `Uchar u -> (match acc with Some _ -> acc | None -> Some u)
      | `Malformed _ -> acc)
    None s

let is_unicode_ident_start s =
  match uchar_of_lexeme s with
  | Some u -> Uucp.Id.is_id_start u
  | None -> false

let is_unicode_ident_continue s =
  String.equal s "."
  ||
  match uchar_of_lexeme s with
  | Some u -> Uucp.Id.is_id_continue u
  | None -> false

let multi_char_operators =
  [%sedlex.regexp?
    ( ">=" | "<=" | "<-" | "->" | "<<-" | "->>" | ":::" | "::" | "|>" | "=="
    | "!=" | "%%" | "||" )]

let comment = [%sedlex.regexp? "#", Star (Compl eof)]
let operators = [%sedlex.regexp? Chars "+-*/%^&|=<>!$@~?:"]

let read_double_quote prev buf =
  let open Sedlexing in
  let rec loop acc buf =
    match%sedlex buf with
    | escaped_char -> loop (Utf8.lexeme buf :: acc) buf
    | '"' -> (STRING ((acc |> List.rev |> String.concat "") ^ "\""), Normal)
    | any -> loop (Utf8.lexeme buf :: acc) buf
    | eof ->
        if acc = [ "" ] then (EOF, In_double_quote "")
        else (STRING (acc |> List.rev |> String.concat ""), In_double_quote "")
    | _ -> failwith "unreachable"
  in
  loop [ prev ] buf

let read_single_quote prev buf =
  let open Sedlexing in
  let rec loop acc buf =
    match%sedlex buf with
    | escaped_char -> loop (Utf8.lexeme buf :: acc) buf
    | '\'' -> (STRING ((acc |> List.rev |> String.concat "") ^ "'"), Normal)
    | any -> loop (Utf8.lexeme buf :: acc) buf
    | eof ->
        if acc = [ prev ] then (EOF, In_single_quote prev)
        else (STRING (acc |> List.rev |> String.concat ""), In_single_quote "")
    | _ -> failwith "unreachable"
  in
  loop [ prev ] buf

let read_backtick prev buf =
  let open Sedlexing in
  let rec loop acc buf =
    match%sedlex buf with
    | escaped_char -> loop (Utf8.lexeme buf :: acc) buf
    | '`' ->
        (BACKTICK_IDENT ((acc |> List.rev |> String.concat "") ^ "`"), Normal)
    | any -> loop (Utf8.lexeme buf :: acc) buf
    | eof ->
        if acc = [ prev ] then (EOF, In_backtick prev)
        else
          (BACKTICK_IDENT (acc |> List.rev |> String.concat ""), In_backtick "")
    | _ -> failwith "unreachable"
  in
  loop [ prev ] buf

let closing_bracket = function
  | "(" -> ")"
  | "[" -> "]"
  | "{" -> "}"
  | _ -> failwith "invalid bracket"

let read_raw_string closing_seq prev buf =
  let open Sedlexing in
  let close_len = String.length closing_seq in
  let rec loop acc buf =
    match%sedlex buf with
    | any ->
        let c = Utf8.lexeme buf in
        let acc' = c :: acc in
        let current = acc' |> List.rev |> String.concat "" in
        if
          String.length current >= close_len
          && String.sub current (String.length current - close_len) close_len
             = closing_seq
        then (STRING (prev ^ current), Normal)
        else loop acc' buf
    | eof ->
        let current = acc |> List.rev |> String.concat "" in
        if current = "" && prev = "" then
          (EOF, In_raw_string (closing_seq, prev))
        else (STRING (prev ^ current), In_raw_string (closing_seq, ""))
    | _ -> failwith "unreachable"
  in
  loop [] buf

let read_raw_string_start prefix buf =
  let open Sedlexing in
  let rec get_delim acc buf =
    match%sedlex buf with
    | '(' | '[' | '{' ->
        let bracket = Utf8.lexeme buf in
        let delim = acc |> List.rev |> String.concat "" in
        let closing_seq = closing_bracket bracket ^ delim ^ "\"" in
        let full_prefix = prefix ^ delim ^ bracket in
        read_raw_string closing_seq full_prefix buf
    | eof ->
        let delim = acc |> List.rev |> String.concat "" in
        (STRING (prefix ^ delim), In_raw_string ("", prefix ^ delim))
    | any -> get_delim (Utf8.lexeme buf :: acc) buf
    | _ -> failwith "unreachable"
  in
  get_delim [] buf

let read_normal buf =
  let open Sedlexing in
  match%sedlex buf with
  | comment -> (COMMENT (Utf8.lexeme buf), Normal)
  | ('r' | 'R'), '"' -> read_raw_string_start (Utf8.lexeme buf) buf
  | '"' -> read_double_quote "\"" buf
  | '\'' -> read_single_quote "'" buf
  | '`' -> read_backtick "`" buf
  | '\\', '(' -> (LAMBDA, Normal)
  | r_number -> (NUMBER (Utf8.lexeme buf), Normal)
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
  | constant -> (CONSTANT (Utf8.lexeme buf), Normal)
  | ident -> (
      let s = Utf8.lexeme buf in
      match keyword_of_string s with
      | Some kw -> (KEYWORD kw, Normal)
      | None -> (IDENT s, Normal))
  | eof -> (EOF, Normal)
  | any ->
      let s = Utf8.lexeme buf in
      if is_unicode_ident_start s then
        let rec read_unicode_ident acc =
          match%sedlex buf with
          | any ->
              let next = Utf8.lexeme buf in
              if is_unicode_ident_continue next then
                read_unicode_ident (next :: acc)
              else (
                rollback buf;
                (IDENT (acc |> List.rev |> String.concat ""), Normal))
          | eof -> (IDENT (acc |> List.rev |> String.concat ""), Normal)
          | _ -> failwith "unreachable"
        in
        read_unicode_ident [ s ]
      else (UNKNOWN s, Normal)
  | _ -> failwith "no match"

let token mode buf =
  match mode with
  | Normal -> read_normal buf
  | In_double_quote prev -> read_double_quote prev buf
  | In_single_quote prev -> read_single_quote prev buf
  | In_backtick prev -> read_backtick prev buf
  | In_raw_string (closing_seq, prev) -> read_raw_string closing_seq prev buf

let string_of_keyword = function
  | IF -> "if"
  | ELSE -> "else"
  | FOR -> "for"
  | WHILE -> "while"
  | REPEAT -> "repeat"
  | IN -> "in"
  | FUNCTION -> "function"
  | BREAK -> "break"
  | NEXT -> "next"
  | RETURN -> "return"

let print_type = function
  | NUMBER s -> Printf.sprintf "NUMBER: %s" s
  | STRING s -> Printf.sprintf "STRING: %s`" s
  | COMMENT s -> Printf.sprintf "COMMENT: %s" s
  | KEYWORD kw -> Printf.sprintf "KEYWORD: %s" (string_of_keyword kw)
  | CONSTANT s -> Printf.sprintf "CONSTANT: %s" s
  | OPERATOR s -> Printf.sprintf "OPERATOR: %s" s
  | IDENT s -> Printf.sprintf "IDENT: %s" s
  | BACKTICK_IDENT s -> Printf.sprintf "BACKTICK_IDENT: %s" s
  | PUNCTUATION s -> Printf.sprintf "PUNCTUATION: %s" s
  | LAMBDA -> "LAMBDA"
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACKET -> "LEFT_BRACKET"
  | RIGHT_BRACKET -> "RIGHT_BRACKET"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | WHITESPACE s -> Printf.sprintf "WHITESPACE: %s" s
  | UNKNOWN s -> Printf.sprintf "UNKNOWN: %s" s
  | DEFAULT s -> Printf.sprintf "DEFAULT: %s" s
  | EOF -> "EOF"

let token_to_lexeme : token -> string = function
  | NUMBER s -> s
  | STRING s -> s
  | COMMENT s -> s
  | KEYWORD kw -> string_of_keyword kw
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
  | DEFAULT s -> s
  | EOF -> ""

let lex_line mode line =
  let for_lexer = Sedlexing.Utf8.from_string line in
  let rec loop acc mode buf =
    if List.length acc > 1000 then
      failwith
        (Printf.sprintf "too much iteration: %s"
           (acc |> List.map print_type |> String.concat ", "))
    else
      match token mode buf with
      | EOF, m -> (List.rev acc, m)
      | LAMBDA, m ->
          (* Expand \(. into lambda token plus left paren for balance logic. *)
          loop (LEFT_PAREN :: LAMBDA :: acc) m buf
      | t, m -> loop (t :: acc) m buf
  in
  loop [] mode for_lexer

let lex_as_default line = DEFAULT line
