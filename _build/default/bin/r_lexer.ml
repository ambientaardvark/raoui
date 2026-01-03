(** Simple lexer for R syntax highlighting.

    This is a hand-rolled lexer for tokenizing R code.
    It's intentionally simple - just enough for highlighting. *)

type token =
  | Keyword of string
  | Identifier of string
  | Number of string
  | String of string
  | Comment of string
  | Operator of string
  | Paren of char
  | Newline
  | Whitespace of string
  | Unknown of char

let keywords = [
  "if"; "else"; "repeat"; "while"; "function"; "for"; "in";
  "next"; "break"; "TRUE"; "FALSE"; "NULL"; "Inf"; "NaN";
  "NA"; "NA_integer_"; "NA_real_"; "NA_complex_"; "NA_character_";
  "library"; "require"; "return"
]

let is_keyword s = List.mem s keywords

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_ident_start c = is_alpha c || c = '.' || c = '_'
let is_ident_char c = is_ident_start c || is_digit c

let is_operator c =
  String.contains "+-*/<>=!&|^~$@:%" c

type lexer = {
  input : string;
  mutable pos : int;
}

let make input = { input; pos = 0 }

let peek lex =
  if lex.pos >= String.length lex.input then None
  else Some lex.input.[lex.pos]

let advance lex =
  if lex.pos < String.length lex.input then
    lex.pos <- lex.pos + 1

let take_while lex pred =
  let start = lex.pos in
  while Option.fold ~none:false ~some:pred (peek lex) do
    advance lex
  done;
  String.sub lex.input start (lex.pos - start)

let read_string lex quote =
  let buf = Buffer.create 32 in
  Buffer.add_char buf quote;
  advance lex;  (* skip opening quote *)
  let rec loop () =
    match peek lex with
    | None -> ()  (* unterminated string *)
    | Some '\\' ->
      Buffer.add_char buf '\\';
      advance lex;
      (match peek lex with
       | Some c -> Buffer.add_char buf c; advance lex
       | None -> ());
      loop ()
    | Some c when c = quote ->
      Buffer.add_char buf c;
      advance lex
    | Some c ->
      Buffer.add_char buf c;
      advance lex;
      loop ()
  in
  loop ();
  String (Buffer.contents buf)

let read_number lex =
  let s = take_while lex (fun c -> is_digit c || c = '.' || c = 'e' || c = 'E' || c = '-' || c = '+' || c = 'L') in
  Number s

let read_identifier lex =
  let s = take_while lex is_ident_char in
  if is_keyword s then Keyword s else Identifier s

let read_comment lex =
  let s = take_while lex (fun c -> c <> '\n') in
  Comment s

let read_operator lex =
  let s = take_while lex is_operator in
  Operator s

let next_token lex =
  match peek lex with
  | None -> None
  | Some c ->
    let tok = match c with
      | '\n' -> advance lex; Newline
      | ' ' | '\t' | '\r' ->
        Whitespace (take_while lex (fun c -> c = ' ' || c = '\t' || c = '\r'))
      | '#' -> read_comment lex
      | '"' | '\'' -> read_string lex c
      | '(' | ')' | '[' | ']' | '{' | '}' ->
        advance lex; Paren c
      | c when is_digit c -> read_number lex
      | c when is_ident_start c -> read_identifier lex
      | c when is_operator c -> read_operator lex
      | c -> advance lex; Unknown c
    in
    Some tok

let tokenize input =
  let lex = make input in
  let rec loop acc =
    match next_token lex with
    | None -> List.rev acc
    | Some tok -> loop (tok :: acc)
  in
  loop []
