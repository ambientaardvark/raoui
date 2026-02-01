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
  | RL.KEYWORD kw -> RL.string_of_keyword kw
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
    (match m1 with RL.In_double_quote _ -> true | _ -> false);

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

(* Helper to check a single token *)
let check_single_token name input expected =
  let tokens, _ = lex input RL.Normal in
  let non_ws = List.filter (function RL.WHITESPACE _ -> false | _ -> true) tokens in
  check int (name ^ " token count") 1 (List.length non_ws);
  check string (name ^ " value") expected (token_lexeme (List.hd non_ws))

let is_constant = function RL.CONSTANT _ -> true | _ -> false

(* Number tests *)
let test_integer () =
  check_single_token "integer" "42" "42"

let test_float () =
  check_single_token "float" "3.14" "3.14"

let test_float_leading_dot () =
  check_single_token "leading dot float" ".5" ".5"

let test_float_trailing_dot () =
  check_single_token "trailing dot float" "5." "5."

let test_hex_number () =
  check_single_token "hex" "0xff" "0xff"

let test_binary_number () =
  check_single_token "binary" "0b101" "0b101"

let test_scientific_notation () =
  check_single_token "scientific" "1e10" "1e10"

let test_scientific_negative_exp () =
  check_single_token "scientific neg exp" "1e-5" "1e-5"

let test_imaginary_number () =
  check_single_token "imaginary" "2i" "2i"

let test_complex_number () =
  check_single_token "complex" "1+2i" "1+2i"

let test_complex_imaginary_first () =
  check_single_token "complex imag first" "2i+1" "2i+1"

let test_integer_suffix () =
  check_single_token "integer L suffix" "42L" "42L"

(* String tests *)
let test_double_quote_string () =
  check_single_token "double quote string" "\"hello\"" "\"hello\""

let test_single_quote_string () =
  check_single_token "single quote string" "'hello'" "'hello'"

let test_string_with_escape () =
  check_single_token "escaped quote" "\"say \\\"hi\\\"\"" "\"say \\\"hi\\\"\""

let test_empty_string () =
  check_single_token "empty string" "\"\"" "\"\""

(* Raw string tests *)
let test_raw_string_basic () =
  check_single_token "raw string basic" "r\"(hello)\"" "r\"(hello)\""

let test_raw_string_with_delimiter () =
  check_single_token "raw string delimiter" "r\"---(hello)---\"" "r\"---(hello)---\""

let test_raw_string_with_quotes () =
  check_single_token "raw string with quotes" "r\"(say \"hi\")\"" "r\"(say \"hi\")\""

let test_raw_string_uppercase () =
  check_single_token "raw string uppercase R" "R\"(hello)\"" "R\"(hello)\""

let test_raw_string_brackets () =
  check_single_token "raw string square brackets" "r\"[hello]\"" "r\"[hello]\""

let test_raw_string_braces () =
  check_single_token "raw string braces" "r\"{hello}\"" "r\"{hello}\""

(* Identifier tests *)
let test_simple_ident () =
  check_single_token "simple ident" "foo" "foo"

let test_ident_with_dots () =
  check_single_token "ident with dots" "data.frame" "data.frame"

let test_ident_with_underscore () =
  check_single_token "ident with underscore" "my_var" "my_var"

let test_dot_ident () =
  check_single_token "dot ident" ".Internal" ".Internal"

let test_ellipsis () =
  check_single_token "ellipsis" "..." "..."

(* Backtick identifier tests *)
let test_backtick_ident () =
  check_single_token "backtick ident" "`my var`" "`my var`"

let test_backtick_with_spaces () =
  check_single_token "backtick with special" "`1st column`" "`1st column`"

(* Constant tests *)
let test_true () =
  let tokens, _ = lex "TRUE" RL.Normal in
  check bool "TRUE is constant" true (is_constant (List.hd tokens))

let test_false () =
  let tokens, _ = lex "FALSE" RL.Normal in
  check bool "FALSE is constant" true (is_constant (List.hd tokens))

let test_null () =
  let tokens, _ = lex "NULL" RL.Normal in
  check bool "NULL is constant" true (is_constant (List.hd tokens))

let test_na () =
  let tokens, _ = lex "NA" RL.Normal in
  check bool "NA is constant" true (is_constant (List.hd tokens))

let test_inf () =
  let tokens, _ = lex "Inf" RL.Normal in
  check bool "Inf is constant" true (is_constant (List.hd tokens))

let test_nan () =
  let tokens, _ = lex "NaN" RL.Normal in
  check bool "NaN is constant" true (is_constant (List.hd tokens))

let test_na_integer () =
  let tokens, _ = lex "NA_integer_" RL.Normal in
  check bool "NA_integer_ is constant" true (is_constant (List.hd tokens))

(* Keyword tests *)
let is_keyword = function RL.KEYWORD _ -> true | _ -> false

let test_keyword_if () =
  let tokens, _ = lex "if" RL.Normal in
  check bool "if is keyword" true (is_keyword (List.hd tokens));
  check bool "if is IF" true (List.hd tokens = RL.KEYWORD RL.IF)

let test_keyword_else () =
  let tokens, _ = lex "else" RL.Normal in
  check bool "else is ELSE" true (List.hd tokens = RL.KEYWORD RL.ELSE)

let test_keyword_for () =
  let tokens, _ = lex "for" RL.Normal in
  check bool "for is FOR" true (List.hd tokens = RL.KEYWORD RL.FOR)

let test_keyword_while () =
  let tokens, _ = lex "while" RL.Normal in
  check bool "while is WHILE" true (List.hd tokens = RL.KEYWORD RL.WHILE)

let test_keyword_function () =
  let tokens, _ = lex "function" RL.Normal in
  check bool "function is FUNCTION" true (List.hd tokens = RL.KEYWORD RL.FUNCTION)

let test_keyword_in () =
  let tokens, _ = lex "in" RL.Normal in
  check bool "in is IN" true (List.hd tokens = RL.KEYWORD RL.IN)

let test_keyword_return () =
  let tokens, _ = lex "return" RL.Normal in
  check bool "return is RETURN" true (List.hd tokens = RL.KEYWORD RL.RETURN)

(* Operator tests *)
let test_assignment () =
  check_single_token "assignment" "<-" "<-"

let test_right_assignment () =
  check_single_token "right assignment" "->" "->"

let test_double_colon () =
  check_single_token "namespace" "::" "::"

let test_triple_colon () =
  check_single_token "internal namespace" ":::" ":::"

let test_pipe () =
  check_single_token "pipe" "|>" "|>"

let test_equality () =
  check_single_token "equality" "==" "=="

let test_inequality () =
  check_single_token "inequality" "!=" "!="

let test_dollar () =
  check_single_token "dollar" "$" "$"

let test_at () =
  check_single_token "at" "@" "@"

let test_tilde () =
  check_single_token "tilde" "~" "~"

let test_question () =
  check_single_token "question" "?" "?"

let test_colon () =
  check_single_token "colon" ":" ":"

(* Punctuation tests *)
let test_comma () =
  check_single_token "comma" "," ","

let test_semicolon () =
  check_single_token "semicolon" ";" ";"

(* Bracket tests *)
let test_parens () =
  let tokens, _ = lex "()" RL.Normal in
  check int "parens count" 2 (List.length tokens);
  check bool "left paren" true (List.hd tokens = RL.LEFT_PAREN);
  check bool "right paren" true (List.nth tokens 1 = RL.RIGHT_PAREN)

let test_brackets () =
  let tokens, _ = lex "[]" RL.Normal in
  check int "brackets count" 2 (List.length tokens);
  check bool "left bracket" true (List.hd tokens = RL.LEFT_BRACKET);
  check bool "right bracket" true (List.nth tokens 1 = RL.RIGHT_BRACKET)

let test_braces () =
  let tokens, _ = lex "{}" RL.Normal in
  check int "braces count" 2 (List.length tokens);
  check bool "left brace" true (List.hd tokens = RL.LEFT_BRACE);
  check bool "right brace" true (List.nth tokens 1 = RL.RIGHT_BRACE)

(* Multiline tests *)
let test_multiline_single_quote () =
  let line1 = "'abc" in
  let _t1, m1 = lex line1 RL.Normal in
  check bool "unterminated single quote" true
    (match m1 with RL.In_single_quote _ -> true | _ -> false);
  let line2 = "def'" in
  let _t2, m2 = lex line2 m1 in
  check bool "single quote closes" true (m2 = RL.Normal)

let test_multiline_backtick () =
  let line1 = "`abc" in
  let _t1, m1 = lex line1 RL.Normal in
  check bool "unterminated backtick" true
    (match m1 with RL.In_backtick _ -> true | _ -> false);
  let line2 = "def`" in
  let _t2, m2 = lex line2 m1 in
  check bool "backtick closes" true (m2 = RL.Normal)

let test_multiline_raw_string () =
  let line1 = "r\"(abc" in
  let _t1, m1 = lex line1 RL.Normal in
  check bool "unterminated raw string" true
    (match m1 with RL.In_raw_string _ -> true | _ -> false);
  let line2 = "def)\"" in
  let _t2, m2 = lex line2 m1 in
  check bool "raw string closes" true (m2 = RL.Normal)

(* Roundtrip tests *)
let test_roundtrip_complex () =
  let s = "df$col <- function(x, ...) { x %>% sum() }" in
  let tokens, _ = lex s RL.Normal in
  check string "complex roundtrip" s (roundtrip tokens)

let test_roundtrip_strings () =
  let s = "c(\"hello\", 'world', `name`)" in
  let tokens, _ = lex s RL.Normal in
  check string "strings roundtrip" s (roundtrip tokens)

let test_roundtrip_numbers () =
  let s = "c(1, 2.5, .3, 4., 1e10, 0xff, 2i, 42L)" in
  let tokens, _ = lex s RL.Normal in
  check string "numbers roundtrip" s (roundtrip tokens)

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
      ( "numbers",
        [
          test_case "integer" `Quick test_integer;
          test_case "float" `Quick test_float;
          test_case "leading dot float" `Quick test_float_leading_dot;
          test_case "trailing dot float" `Quick test_float_trailing_dot;
          test_case "hex" `Quick test_hex_number;
          test_case "binary" `Quick test_binary_number;
          test_case "scientific" `Quick test_scientific_notation;
          test_case "scientific negative exp" `Quick test_scientific_negative_exp;
          test_case "imaginary" `Quick test_imaginary_number;
          test_case "complex" `Quick test_complex_number;
          test_case "complex imaginary first" `Quick test_complex_imaginary_first;
          test_case "integer L suffix" `Quick test_integer_suffix;
        ] );
      ( "strings",
        [
          test_case "double quote" `Quick test_double_quote_string;
          test_case "single quote" `Quick test_single_quote_string;
          test_case "escape sequence" `Quick test_string_with_escape;
          test_case "empty string" `Quick test_empty_string;
        ] );
      ( "raw_strings",
        [
          test_case "basic" `Quick test_raw_string_basic;
          test_case "with delimiter" `Quick test_raw_string_with_delimiter;
          test_case "with quotes inside" `Quick test_raw_string_with_quotes;
          test_case "uppercase R" `Quick test_raw_string_uppercase;
          test_case "square brackets" `Quick test_raw_string_brackets;
          test_case "braces" `Quick test_raw_string_braces;
        ] );
      ( "identifiers",
        [
          test_case "simple" `Quick test_simple_ident;
          test_case "with dots" `Quick test_ident_with_dots;
          test_case "with underscore" `Quick test_ident_with_underscore;
          test_case "dot prefix" `Quick test_dot_ident;
          test_case "ellipsis" `Quick test_ellipsis;
        ] );
      ( "backtick_idents",
        [
          test_case "basic" `Quick test_backtick_ident;
          test_case "with spaces" `Quick test_backtick_with_spaces;
        ] );
      ( "constants",
        [
          test_case "TRUE" `Quick test_true;
          test_case "FALSE" `Quick test_false;
          test_case "NULL" `Quick test_null;
          test_case "NA" `Quick test_na;
          test_case "Inf" `Quick test_inf;
          test_case "NaN" `Quick test_nan;
          test_case "NA_integer_" `Quick test_na_integer;
        ] );
      ( "keywords",
        [
          test_case "if" `Quick test_keyword_if;
          test_case "else" `Quick test_keyword_else;
          test_case "for" `Quick test_keyword_for;
          test_case "while" `Quick test_keyword_while;
          test_case "function" `Quick test_keyword_function;
          test_case "in" `Quick test_keyword_in;
          test_case "return" `Quick test_keyword_return;
        ] );
      ( "operators",
        [
          test_case "assignment" `Quick test_assignment;
          test_case "right assignment" `Quick test_right_assignment;
          test_case "double colon" `Quick test_double_colon;
          test_case "triple colon" `Quick test_triple_colon;
          test_case "pipe" `Quick test_pipe;
          test_case "equality" `Quick test_equality;
          test_case "inequality" `Quick test_inequality;
          test_case "dollar" `Quick test_dollar;
          test_case "at" `Quick test_at;
          test_case "tilde" `Quick test_tilde;
          test_case "question" `Quick test_question;
          test_case "colon" `Quick test_colon;
        ] );
      ( "punctuation",
        [
          test_case "comma" `Quick test_comma;
          test_case "semicolon" `Quick test_semicolon;
        ] );
      ( "brackets",
        [
          test_case "parens" `Quick test_parens;
          test_case "brackets" `Quick test_brackets;
          test_case "braces" `Quick test_braces;
        ] );
      ( "multiline",
        [
          test_case "single quote" `Quick test_multiline_single_quote;
          test_case "backtick" `Quick test_multiline_backtick;
          test_case "raw string" `Quick test_multiline_raw_string;
        ] );
      ( "roundtrip",
        [
          test_case "complex expression" `Quick test_roundtrip_complex;
          test_case "various strings" `Quick test_roundtrip_strings;
          test_case "various numbers" `Quick test_roundtrip_numbers;
        ] );
    ]
