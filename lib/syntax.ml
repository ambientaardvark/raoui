open Terminal_ops

(** {1 Lexer cache instantiation} *)

(** Instantiate the generic incremental lexer cache for R *)
module Cache = Language_syntax.Make (struct
  type token = R_lexer.token
  type mode = R_lexer.mode

  let initial_mode = R_lexer.Normal
  let lex_line = R_lexer.lex_line
  let lex_as_default = R_lexer.lex_as_default
end)

(** {1 Syntax highlighting} *)

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
  | DEFAULT _ -> `Plain
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
  | DEFAULT s -> s
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
              else
                match inner.[p] with '{' | '}' -> p | _ -> find_brace (p + 1)
            in
            let end_pos = find_brace pos in
            parse
              ((`String, String.sub inner pos (end_pos - pos)) :: acc)
              end_pos
    in
    let inner_spans = parse [] 0 in
    ((`String, "\"") :: inner_spans)
    @ if has_close then [ (`String, "\"") ] else []

(** Convert a token list to spans, with lookahead for function detection *)
let tokens_to_spans (tokens : Lexer.token list) : span list =
  let rec loop acc = function
    | [] -> List.rev acc
    | Lexer.IDENT f :: Lexer.LEFT_PAREN :: Lexer.STRING s :: tl
      when f = "glue" || f = "str_glue" ->
        let glue_spans = parse_glue_string s in
        loop
          (List.rev_append glue_spans
             ((`Bracket, "(") :: (`Function, f) :: acc))
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

(** {1 Continuation detection} *)

(** R-specific continuation detection - determines if more input is needed *)
module Continuation = struct
  type signal =
    | Submit
    | Continue of { indent_levels : int; in_empty_brackets : bool }

  type pending_control = {
    needs_header : bool;
    header_depth : int;
    header_done : bool;
  }
  (** Internal state for tracking control flow constructs *)

  type continuation_state = {
    parens : int;
    brackets : int;
    braces : int;
    last_significant : R_lexer.token option;
    pending : pending_control option;
  }
  (** State accumulated while analyzing tokens *)

  (** Tokens that should be ignored in continuation analysis *)
  let is_ignorable = function
    | R_lexer.WHITESPACE _ | R_lexer.COMMENT _ -> true
    | _ -> false

  (** Control flow keywords that may require continuation *)
  let is_control_keyword = function
    | R_lexer.IF | R_lexer.ELSE | R_lexer.FOR | R_lexer.WHILE | R_lexer.REPEAT
    | R_lexer.FUNCTION ->
        true
    | _ -> false

  (** Determine pending state for a control keyword *)
  let pending_for_keyword = function
    | R_lexer.IF | R_lexer.FOR | R_lexer.WHILE | R_lexer.FUNCTION ->
        Some { needs_header = true; header_depth = 0; header_done = false }
    | R_lexer.ELSE | R_lexer.REPEAT ->
        Some { needs_header = false; header_depth = 0; header_done = true }
    | _ -> None

  (** Check if token starts an expression *)
  let is_expr_token = function
    | R_lexer.NUMBER _ | R_lexer.STRING _ | R_lexer.CONSTANT _ | R_lexer.IDENT _
    | R_lexer.BACKTICK_IDENT _ | R_lexer.LAMBDA | R_lexer.UNKNOWN _ ->
        true
    | R_lexer.KEYWORD kw -> not (is_control_keyword kw)
    | R_lexer.LEFT_PAREN | R_lexer.LEFT_BRACKET -> true
    | _ -> false

  (** Check if token requires continuation *)
  let is_trailing_token = function
    | R_lexer.OPERATOR _ -> true
    | R_lexer.LEFT_PAREN | R_lexer.LEFT_BRACKET | R_lexer.LEFT_BRACE -> true
    | R_lexer.PUNCTUATION "," -> true
    | R_lexer.KEYWORD kw when is_control_keyword kw || kw = R_lexer.IN -> true
    | _ -> false

  (** Update pending state when processing control structure headers *)
  let update_pending_for_header pending tok =
    match pending with
    | None -> None
    | Some p when p.needs_header && not p.header_done -> (
        match tok with
        | R_lexer.LEFT_PAREN ->
            Some { p with header_depth = p.header_depth + 1 }
        | R_lexer.RIGHT_PAREN ->
            if p.header_depth > 0 then
              let depth = p.header_depth - 1 in
              let header_done = depth = 0 in
              Some { p with header_depth = depth; header_done }
            else Some p
        | _ -> Some p)
    | Some p -> Some p

  (** Fold over tokens to build continuation state *)
  let fold_continuation_state tokens =
    let step state tok =
      if is_ignorable tok then state
      else
        let state = { state with last_significant = Some tok } in
        let state =
          match tok with
          | R_lexer.KEYWORD kw -> (
              match pending_for_keyword kw with
              | Some pending -> { state with pending = Some pending }
              | None -> state)
          | R_lexer.LEFT_PAREN ->
              let pending = update_pending_for_header state.pending tok in
              { state with parens = state.parens + 1; pending }
          | R_lexer.RIGHT_PAREN ->
              let pending = update_pending_for_header state.pending tok in
              { state with parens = max 0 (state.parens - 1); pending }
          | R_lexer.LEFT_BRACKET -> { state with brackets = state.brackets + 1 }
          | R_lexer.RIGHT_BRACKET ->
              { state with brackets = max 0 (state.brackets - 1) }
          | R_lexer.LEFT_BRACE ->
              { state with braces = state.braces + 1; pending = None }
          | R_lexer.RIGHT_BRACE ->
              { state with braces = max 0 (state.braces - 1) }
          | _ -> state
        in
        match state.pending with
        | Some p when p.header_done && is_expr_token tok ->
            { state with pending = None }
        | _ -> state
    in
    let initial =
      {
        parens = 0;
        brackets = 0;
        braces = 0;
        last_significant = None;
        pending = None;
      }
    in
    List.fold_left step initial tokens

  (** Analyze tokens to determine if continuation is needed *)
  let analyze tokens =
    let state = fold_continuation_state tokens in
    let unclosed = state.parens > 0 || state.brackets > 0 || state.braces > 0 in
    let trailing =
      match state.last_significant with
      | Some tok -> is_trailing_token tok
      | None -> false
    in
    let incomplete = Option.is_some state.pending in
    if unclosed || trailing || incomplete then
      let operator_cont =
        match state.last_significant with
        | Some (R_lexer.OPERATOR _) -> true
        | _ -> false
      in
      let indent_levels =
        state.braces + state.parens + state.brackets
        + if operator_cont then 1 else 0
      in
      Continue { indent_levels; in_empty_brackets = false }
    else Submit

  (** Check if cursor is inside empty braces {} *)
  let inside_empty_brackets ~tokens ~cursor_byte_offset =
    let positioned =
      Language_syntax.tokens_with_positions tokens ~token_to_lexeme
    in
    let token_before =
      List.fold_left
        (fun acc (tok, _, end_pos) ->
          if end_pos = cursor_byte_offset then Some tok else acc)
        None positioned
    in
    let token_after =
      List.find_opt
        (fun (_, start_pos, _) -> start_pos = cursor_byte_offset)
        positioned
      |> Option.map (fun (tok, _, _) -> tok)
    in
    match (token_before, token_after) with
    | Some R_lexer.LEFT_BRACE, Some R_lexer.RIGHT_BRACE -> true
    | _ -> false
end
