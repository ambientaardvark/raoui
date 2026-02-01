open Alcotest
module RL = Raoui.R_lexer

(* These tests assume you implement:
   R_lexer.lex_line : R_lexer.mode -> string -> R_lexer.token list * R_lexer.mode

   Token constructors already carry their lexeme (except for bracket/brace/parens),
   so we can test the roundtrip invariant:
   concat(token_lexemes) = original input.
*)

let token_lexeme : RL.token -> string = function
  | RL.NUMBER s -> s
  | RL.STRING s -> s
  | RL.COMMENT s -> s
  | RL.KEYWORD s -> s
  | RL.CONSTANT s -> s
  | RL.OPERATOR s -> s
  | RL.IDENT s -> s
  | RL.BACKTICK_IDENT s -> s
  | RL.PUNCTUATION s -> s
  | RL.LEFT_PAREN -> "("
  | RL.RIGHT_PAREN -> ")"
  | RL.LEFT_BRACKET -> "["
  | RL.RIGHT_BRACKET -> "]"
  | RL.LEFT_BRACE -> "{"
  | RL.RIGHT_BRACE -> "}"
  | RL.WHITESPACE s -> s
  | RL.UNKNOWN s -> s
  | RL.EOF -> ""

let roundtrip tokens = String.concat "" (List.map token_lexeme tokens)

let lex s mode =
  let tokens, mode_out = RL.lex_line mode s in
  (tokens, mode_out)

let test_roundtrip_simple () =
  let s = "x <- 1 + 2" in
  let tokens, _mode_out = lex s RL.Normal in
  check string "concat lexemes = input" s (roundtrip tokens)

let test_multiline_string_mode () =
  let line1 = "\"abc" in
  let _t1, m1 = lex line1 RL.Normal in
  check bool "unterminated double quote carries mode" true
    (m1 = RL.In_double_quote);

  let line2 = "def\"" in
  let _t2, m2 = lex line2 m1 in
  check bool "closing quote returns to Normal" true (m2 = RL.Normal)

let test_percent_operator_single_token () =
  let s = "x %>% f(y)" in
  let tokens, _ = lex s RL.Normal in
  let pct_ops =
    List.filter_map
      (function RL.OPERATOR op when op = "%>%" -> Some op | _ -> None)
      tokens
  in
  check int "find exactly one %>% operator token" 1 (List.length pct_ops)

let () =
  run "r_lexer"
    [
      ( "lexing",
        [
          test_case "roundtrip (simple)" `Quick test_roundtrip_simple;
          test_case "multiline string mode" `Quick test_multiline_string_mode;
          test_case "%...% operator is single token" `Quick
            test_percent_operator_single_token;
        ] );
    ]
