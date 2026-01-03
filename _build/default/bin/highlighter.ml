(** Syntax highlighting for R code using lambda-term styles. *)

open LTerm_style

(** Color scheme - easy to customize *)
let keyword_style = { none with foreground = Some lmagenta; bold = Some true }
let string_style = { none with foreground = Some lgreen }
let number_style = { none with foreground = Some lcyan }
let comment_style = { none with foreground = Some lblue}
let operator_style = { none with foreground = Some lyellow }
let paren_style = { none with foreground = Some lwhite }
let identifier_style = none
let default_style = none

let style_of_token = function
  | R_lexer.Keyword _ -> keyword_style
  | R_lexer.String _ -> string_style
  | R_lexer.Number _ -> number_style
  | R_lexer.Comment _ -> comment_style
  | R_lexer.Operator _ -> operator_style
  | R_lexer.Paren _ -> paren_style
  | R_lexer.Identifier _ -> identifier_style
  | R_lexer.Newline | R_lexer.Whitespace _ | R_lexer.Unknown _ -> default_style

let token_text = function
  | R_lexer.Keyword s | R_lexer.Identifier s | R_lexer.Number s
  | R_lexer.String s | R_lexer.Comment s | R_lexer.Operator s
  | R_lexer.Whitespace s -> s
  | R_lexer.Paren c | R_lexer.Unknown c -> String.make 1 c
  | R_lexer.Newline -> "\n"

(** Convert R code to a styled string for lambda-term *)
let highlight code =
  let tokens = R_lexer.tokenize code in
  let styled_segments =
    tokens
    |> List.map (fun tok ->
        let text = token_text tok in
        let style = style_of_token tok in
        (text, style))
  in
  (* Build a LTerm_text.t *)
  (* let open LTerm_text in *)
  let result = ref [] in
  List.iter (fun (text, style) ->
    String.iter (fun c ->
      result := (Zed_char.of_utf8 (String.make 1 c), style) :: !result
    ) text
  ) styled_segments;
  Array.of_list (List.rev !result)
