type signal =
  | Submit
  | Continue of { indent_levels : int; in_empty_brackets : bool }

type pending_control = {
  needs_header : bool;
  header_depth : int;
  header_done : bool;
}

type continuation_state = {
  parens : int;
  brackets : int;
  braces : int;
  last_significant : R_lexer.token option;
  pending : pending_control option;
}

let is_ignorable = function
  | R_lexer.WHITESPACE _ | R_lexer.COMMENT _ -> true
  | _ -> false

let is_control_keyword = function
  | R_lexer.IF | R_lexer.ELSE | R_lexer.FOR | R_lexer.WHILE | R_lexer.REPEAT
  | R_lexer.FUNCTION ->
      true
  | _ -> false

let pending_for_keyword = function
  | R_lexer.IF | R_lexer.FOR | R_lexer.WHILE | R_lexer.FUNCTION ->
      Some { needs_header = true; header_depth = 0; header_done = false }
  | R_lexer.ELSE | R_lexer.REPEAT ->
      Some { needs_header = false; header_depth = 0; header_done = true }
  | _ -> None

let is_expr_token = function
  | R_lexer.NUMBER _ | R_lexer.STRING _ | R_lexer.CONSTANT _ | R_lexer.IDENT _
  | R_lexer.BACKTICK_IDENT _ | R_lexer.LAMBDA | R_lexer.UNKNOWN _ ->
      true
  | R_lexer.KEYWORD kw -> not (is_control_keyword kw)
  | R_lexer.LEFT_PAREN | R_lexer.LEFT_BRACKET -> true
  | _ -> false

let is_trailing_token = function
  | R_lexer.OPERATOR _ -> true
  | R_lexer.LEFT_PAREN | R_lexer.LEFT_BRACKET | R_lexer.LEFT_BRACE -> true
  | R_lexer.PUNCTUATION "," -> true
  | R_lexer.KEYWORD kw when is_control_keyword kw || kw = R_lexer.IN -> true
  | _ -> false

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

let inside_empty_brackets ~tokens ~cursor_byte_offset =
  let positioned =
    Lexer_cache.tokens_with_positions tokens ~token_to_lexeme:R_lexer.token_to_lexeme
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
