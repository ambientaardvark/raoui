open Alcotest
module R_highlight = Raoui.R_highlight
module Lexer = Raoui.R_lexer

let span_to_string (style, text) =
  let style_name =
    match style with
    | `String -> "String"
    | `Bracket -> "Bracket"
    | `Ident -> "Ident"
    | `Number -> "Number"
    | `Operator -> "Operator"
    | `Keyword -> "Keyword"
    | `Constant -> "Constant"
    | `Function -> "Function"
    | `Comment -> "Comment"
    | `Plain -> "Plain"
    | `Error -> "Error"
    | `Accent -> "Accent"
    | `Raw -> "Raw"
    | `Completion -> "Completion"
    | `Completion_selected -> "Completion_selected"
    | `Shell_prompt -> "Shell_prompt"
  in
  Printf.sprintf "(%s, %S)" style_name text

let spans_to_string spans =
  "[" ^ String.concat "; " (List.map span_to_string spans) ^ "]"

let check_spans name expected actual =
  check string name (spans_to_string expected) (spans_to_string actual)

(* parse_glue_string tests *)

let test_glue_simple () =
  let result = R_highlight.parse_glue_string "\"hello\"" in
  check_spans "simple string"
    [ (`String, "\""); (`String, "hello"); (`String, "\"") ]
    result

let test_glue_interpolation () =
  let result = R_highlight.parse_glue_string "\"hello {name}\"" in
  check_spans "interpolation"
    [
      (`String, "\"");
      (`String, "hello ");
      (`Bracket, "{");
      (`Ident, "name");
      (`Bracket, "}");
      (`String, "\"");
    ]
    result

let test_glue_escaped_open () =
  let result = R_highlight.parse_glue_string "\"{{literal\"" in
  check_spans "escaped {{"
    [ (`String, "\""); (`String, "{{"); (`String, "literal"); (`String, "\"") ]
    result

let test_glue_escaped_close () =
  let result = R_highlight.parse_glue_string "\"literal}}\"" in
  check_spans "escaped }}"
    [ (`String, "\""); (`String, "literal"); (`String, "}}"); (`String, "\"") ]
    result

let test_glue_expr_then_escaped () =
  (* glue("hello, {a}}}") -> hello, 2} *)
  let result = R_highlight.parse_glue_string "\"hello, {a}}}\"" in
  check_spans "expr then escaped"
    [
      (`String, "\"");
      (`String, "hello, ");
      (`Bracket, "{");
      (`Ident, "a");
      (`Bracket, "}");
      (`String, "}}");
      (`String, "\"");
    ]
    result

let test_glue_escaped_then_expr () =
  (* glue("hello, {{{a}") -> hello, {2 *)
  let result = R_highlight.parse_glue_string "\"hello, {{{a}\"" in
  check_spans "escaped then expr"
    [
      (`String, "\"");
      (`String, "hello, ");
      (`String, "{{");
      (`Bracket, "{");
      (`Ident, "a");
      (`Bracket, "}");
      (`String, "\"");
    ]
    result

let test_glue_nested_braces () =
  (* R code with braces inside interpolation *)
  let result = R_highlight.parse_glue_string "\"{if (x) { 1 } else { 2 }}\"" in
  (* Should find matching } accounting for nesting *)
  let styles = List.map fst result in
  check bool "starts with string quote" true (List.hd styles = `String);
  check bool "has brackets" true (List.mem `Bracket styles);
  check bool "has keyword" true (List.mem `Keyword styles)

let test_glue_multiple_interpolations () =
  let result = R_highlight.parse_glue_string "\"{a} and {b}\"" in
  check_spans "multiple interpolations"
    [
      (`String, "\"");
      (`Bracket, "{");
      (`Ident, "a");
      (`Bracket, "}");
      (`String, " and ");
      (`Bracket, "{");
      (`Ident, "b");
      (`Bracket, "}");
      (`String, "\"");
    ]
    result

let test_glue_complex_expr () =
  let result = R_highlight.parse_glue_string "\"{x + 1}\"" in
  check_spans "complex expr"
    [
      (`String, "\"");
      (`Bracket, "{");
      (`Ident, "x");
      (`Plain, " ");
      (`Operator, "+");
      (`Plain, " ");
      (`Number, "1");
      (`Bracket, "}");
      (`String, "\"");
    ]
    result

(* tokens_to_spans tests *)

let test_function_detection () =
  let tokens, _ = Lexer.lex_line Lexer.Normal "foo(x)" in
  let spans = R_highlight.tokens_to_spans tokens in
  let styles = List.map fst spans in
  check bool "function style detected" true (List.hd styles = `Function);
  check bool "has bracket" true (List.mem `Bracket styles)

let test_glue_in_tokens_to_spans () =
  let tokens, _ = Lexer.lex_line Lexer.Normal "glue(\"hi {x}\")" in
  let spans = R_highlight.tokens_to_spans tokens in
  let styles = List.map fst spans in
  check bool "glue is function" true (List.hd styles = `Function);
  check bool "has bracket for interpolation" true
    (List.filter (fun s -> s = `Bracket) styles |> List.length >= 3)

(* highlight_line tests *)

let test_highlight_simple () =
  let spans, mode = R_highlight.highlight_line Lexer.Normal "x <- 1" in
  check bool "returns Normal mode" true (mode = Lexer.Normal);
  let text = String.concat "" (List.map snd spans) in
  check string "roundtrip" "x <- 1" text

let test_highlight_with_glue () =
  let spans, _ = R_highlight.highlight_line Lexer.Normal "glue(\"{x}\")" in
  let text = String.concat "" (List.map snd spans) in
  check string "roundtrip with glue" "glue(\"{x}\")" text

let () =
  run "r_highlight"
    [
      ( "parse_glue_string",
        [
          test_case "simple string" `Quick test_glue_simple;
          test_case "interpolation" `Quick test_glue_interpolation;
          test_case "escaped {{" `Quick test_glue_escaped_open;
          test_case "escaped }}" `Quick test_glue_escaped_close;
          test_case "expr then escaped" `Quick test_glue_expr_then_escaped;
          test_case "escaped then expr" `Quick test_glue_escaped_then_expr;
          test_case "nested braces" `Quick test_glue_nested_braces;
          test_case "multiple interpolations" `Quick
            test_glue_multiple_interpolations;
          test_case "complex expr" `Quick test_glue_complex_expr;
        ] );
      ( "tokens_to_spans",
        [
          test_case "function detection" `Quick test_function_detection;
          test_case "glue detection" `Quick test_glue_in_tokens_to_spans;
        ] );
      ( "highlight_line",
        [
          test_case "simple" `Quick test_highlight_simple;
          test_case "with glue" `Quick test_highlight_with_glue;
        ] );
    ]
