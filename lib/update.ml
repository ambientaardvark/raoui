open Frontend_types

let lex_cache_for_lines lines =
  let rec loop mode acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let text = Unicode_string.to_string line in
        let tokens, end_mode = R_lexer.lex_line mode text in
        let entry = { text; tokens; start_mode = mode; end_mode } in
        loop end_mode (entry :: acc) rest
  in
  loop R_lexer.Normal [] lines

let lexer_update start end_ model =
  if List.length model.lines <> List.length model.lex_cache then
    { model with lex_cache = lex_cache_for_lines model.lines }
  else
    let rec loop i mode lines cache acc =
      match lines with
      | [] -> List.rev acc
      | line :: rest ->
          if i < start then
            match cache with
            | cached :: cache_rest ->
                loop (i + 1) cached.end_mode rest cache_rest (cached :: acc)
            | [] -> List.rev acc
          else if i > end_ && R_lexer.(mode = Normal) then
            match cache with
            | cached :: cache_rest when R_lexer.(cached.start_mode = Normal) ->
                List.rev acc @ (cached :: cache_rest)
            | _ ->
                let text = Unicode_string.to_string line in
                let tokens, end_mode = R_lexer.lex_line mode text in
                let entry = { text; tokens; start_mode = mode; end_mode } in
                let cache_rest = match cache with _ :: tl -> tl | [] -> [] in
                loop (i + 1) end_mode rest cache_rest (entry :: acc)
          else
            let text = Unicode_string.to_string line in
            let tokens, end_mode = R_lexer.lex_line mode text in
            let entry = { text; tokens; start_mode = mode; end_mode } in
            let cache_rest = match cache with _ :: tl -> tl | [] -> [] in
            loop (i + 1) end_mode rest cache_rest (entry :: acc)
    in
    {
      model with
      lex_cache = loop 0 R_lexer.Normal model.lines model.lex_cache [];
    }

(* Cursor context primitives - use internal coordinates *)
let current_line model = List.nth model.lines model.cursor_line
let line_length model = Unicode_string.length (current_line model)

let char_at model =
  let line = current_line model in
  if model.cursor_pos >= Unicode_string.length line then None
  else Some (Unicode_string.cluster_at line model.cursor_pos)

let char_before model =
  if model.cursor_pos = 0 then None
  else
    Some (Unicode_string.cluster_at (current_line model) (model.cursor_pos - 1))

let at_line_start model = model.cursor_pos = 0
let at_line_end model = model.cursor_pos >= line_length model
let at_first_line model = model.cursor_line = 0
let at_last_line model = model.cursor_line >= List.length model.lines - 1

let same_cursor_pos m1 m2 =
  m1.cursor_line = m2.cursor_line && m1.cursor_pos = m2.cursor_pos

let insert_char model c =
  let width = effective_width model in
  let line = current_line model in
  match
    Unicode_string.insert_string line ~pos:model.cursor_pos (String.make 1 c)
  with
  | Error _ -> model (* Invalid UTF-8 byte, ignore it *)
  | Ok new_line ->
      let new_lines = update_line model.lines model.cursor_line new_line in
      let new_pos = model.cursor_pos + 1 in
      let new_row, new_col =
        internal_to_terminal width new_lines (model.cursor_line, new_pos)
      in
      {
        model with
        lines = new_lines;
        cursor_pos = new_pos;
        cursor_row = new_row;
        cursor_col = new_col;
      }
      |> lexer_update model.cursor_line model.cursor_line

let delete_char model =
  let width = effective_width model in
  if at_line_start model then
    if at_first_line model then model
    else
      let prev_line = List.nth model.lines (model.cursor_line - 1) in
      let curr_line = current_line model in
      let merged = Unicode_string.append prev_line curr_line in
      let new_lines =
        model.lines
        |> List.filteri (fun i _ -> i <> model.cursor_line)
        |> List.mapi (fun i line ->
            if i = model.cursor_line - 1 then merged else line)
      in
      let new_line_idx = model.cursor_line - 1 in
      let new_pos = Unicode_string.length prev_line in
      let new_row, new_col =
        internal_to_terminal width new_lines (new_line_idx, new_pos)
      in
      {
        model with
        lines = new_lines;
        cursor_line = new_line_idx;
        cursor_pos = new_pos;
        cursor_row = new_row;
        cursor_col = new_col;
      }
      |> lexer_update new_line_idx (new_line_idx + 1)
  else
    let line = current_line model in
    let new_line = Unicode_string.delete line (model.cursor_pos - 1) in
    let new_lines = update_line model.lines model.cursor_line new_line in
    let new_pos = model.cursor_pos - 1 in
    let new_row, new_col =
      internal_to_terminal width new_lines (model.cursor_line, new_pos)
    in
    {
      model with
      lines = new_lines;
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
    |> lexer_update model.cursor_line model.cursor_line

let insert_newline model =
  let width = effective_width model in
  let line = current_line model in
  let before, after = Unicode_string.split line model.cursor_pos in
  let new_lines =
    List.concat_map
      (fun (i, l) -> if i = model.cursor_line then [ before; after ] else [ l ])
      (List.mapi (fun i l -> (i, l)) model.lines)
  in
  let new_line_idx = model.cursor_line + 1 in
  let new_row, new_col =
    internal_to_terminal width new_lines (new_line_idx, 0)
  in
  {
    model with
    lines = new_lines;
    cursor_line = new_line_idx;
    cursor_pos = 0;
    cursor_row = new_row;
    cursor_col = new_col;
  }
  |> lexer_update model.cursor_line new_line_idx

let insert_paste model text =
  let max_len = 5 * 1024 in
  let text =
    if String.length text > max_len then String.sub text 0 max_len else text
  in
  let width = effective_width model in
  let line = current_line model in
  let before, after = Unicode_string.split line model.cursor_pos in
  let paste_lines = String.split_on_char '\n' text in
  let to_us s =
    match Unicode_string.of_string s with
    | Ok u -> u
    | Error _ -> Unicode_string.empty
  in
  let inserted, final_pos =
    match paste_lines with
    | [] -> ([ Unicode_string.append before after ], model.cursor_pos)
    | [ single ] ->
        let single_us = to_us single in
        ( [ Unicode_string.concat [ before; single_us; after ] ],
          model.cursor_pos + Unicode_string.length single_us )
    | first :: rest ->
        let rec split_last = function
          | [] -> ([], "")
          | [ x ] -> ([], x)
          | x :: xs ->
              let middle, last = split_last xs in
              (x :: middle, last)
        in
        let middle, last = split_last rest in
        let first_us = to_us first in
        let last_us = to_us last in
        let middle_us = List.map to_us middle in
        let first_line = Unicode_string.append before first_us in
        let last_line = Unicode_string.append last_us after in
        ( (first_line :: middle_us) @ [ last_line ],
          Unicode_string.length last_us )
  in
  let new_lines =
    List.concat_map
      (fun (i, l) -> if i = model.cursor_line then inserted else [ l ])
      (List.mapi (fun i l -> (i, l)) model.lines)
  in
  let final_line_idx = model.cursor_line + List.length paste_lines - 1 in
  let new_row, new_col =
    internal_to_terminal width new_lines (final_line_idx, final_pos)
  in
  {
    model with
    lines = new_lines;
    cursor_line = final_line_idx;
    cursor_pos = final_pos;
    cursor_row = new_row;
    cursor_col = new_col;
  }
  |> lexer_update model.cursor_line final_line_idx

let move_left model =
  let width = effective_width model in
  if not (at_line_start model) then
    let new_pos = model.cursor_pos - 1 in
    let new_row, new_col =
      internal_to_terminal width model.lines (model.cursor_line, new_pos)
    in
    {
      model with
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else if not (at_first_line model) then
    let new_line = model.cursor_line - 1 in
    let prev_line = List.nth model.lines new_line in
    let new_pos = Unicode_string.length prev_line in
    let new_row, new_col =
      internal_to_terminal width model.lines (new_line, new_pos)
    in
    {
      model with
      cursor_line = new_line;
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else model

let move_right model =
  let width = effective_width model in
  if not (at_line_end model) then
    let new_pos = model.cursor_pos + 1 in
    let new_row, new_col =
      internal_to_terminal width model.lines (model.cursor_line, new_pos)
    in
    {
      model with
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else if not (at_last_line model) then
    let new_line = model.cursor_line + 1 in
    let new_row, new_col =
      internal_to_terminal width model.lines (new_line, 0)
    in
    {
      model with
      cursor_line = new_line;
      cursor_pos = 0;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else model

let move_up model =
  let width = effective_width model in
  let wrapped_lines = wrap_lines width model.lines in
  if model.cursor_row > 0 then
    let target_col =
      match model.previous_key with
      | Some Up | Some Down -> model.persistent_col
      | _ -> model.cursor_col
    in
    let line_width =
      List.nth wrapped_lines (model.cursor_row - 1)
      |> Unicode_string.display_width
    in
    let new_row = model.cursor_row - 1 in
    let new_col = min target_col line_width in
    let new_cursor_line, new_cursor_pos =
      terminal_to_internal width model.lines (new_row, new_col)
    in
    {
      model with
      cursor_row = new_row;
      cursor_col = new_col;
      cursor_line = new_cursor_line;
      cursor_pos = new_cursor_pos;
      persistent_col = target_col;
    }
  else model

let move_down model =
  let width = effective_width model in
  let wrapped_lines = wrap_lines width model.lines in
  let total_rows = List.length wrapped_lines in
  if model.cursor_row < total_rows - 1 then
    let target_col =
      match model.previous_key with
      | Some Up | Some Down -> model.persistent_col
      | _ -> model.cursor_col
    in
    let line_width =
      List.nth wrapped_lines (model.cursor_row + 1)
      |> Unicode_string.display_width
    in
    let new_row = model.cursor_row + 1 in
    let new_col = min target_col line_width in
    let new_cursor_line, new_cursor_pos =
      terminal_to_internal width model.lines (new_row, new_col)
    in
    {
      model with
      cursor_row = new_row;
      cursor_col = new_col;
      cursor_line = new_cursor_line;
      cursor_pos = new_cursor_pos;
      persistent_col = target_col;
    }
  else model

let insert_matched_start model c =
  let after1 = insert_char model c in
  match c with
  | '[' -> move_left (insert_char after1 ']')
  | '(' -> move_left (insert_char after1 ')')
  | '{' -> move_left (insert_char after1 '}')
  | _ -> after1

let insert_matched_end model c =
  if at_line_end model then insert_char model c
  else
    match char_at model with
    | Some s when String.get s 0 = c -> move_right model
    | _ -> insert_char model c

let insert_matched_same model c =
  if at_line_end model then insert_char (insert_char model c) c |> move_left
  else
    match char_at model with
    | Some s when String.get s 0 = c -> move_right model
    | _ -> insert_char (insert_char model c) c |> move_left

let user_input_char model c =
  match c with
  | '[' | '{' | '(' -> insert_matched_start model c
  | ']' | '}' | ')' -> insert_matched_end model c
  | '\'' | '"' -> insert_matched_same model c
  | _ -> insert_char model c

let user_input_delete model =
  if at_line_start model || at_line_end model then delete_char model
  else
    match (char_before model, char_at model) with
    | Some before_s, Some at_s ->
        let b = String.get before_s 0 in
        let a = String.get at_s 0 in
        let is_matched_pair =
          match b with
          | '(' -> a = ')'
          | '[' -> a = ']'
          | '{' -> a = '}'
          | '"' -> a = '"'
          | '\'' -> a = '\''
          | _ -> false
        in
        if is_matched_pair then
          model |> move_right |> delete_char |> delete_char
        else delete_char model
    | _ -> delete_char model

let move_cursor_to_end model =
  let width = effective_width model in
  let last_line_idx = List.length model.lines - 1 in
  let last_line = List.nth model.lines last_line_idx in
  let new_pos = Unicode_string.length last_line in
  let new_row, new_col =
    internal_to_terminal width model.lines (last_line_idx, new_pos)
  in
  {
    model with
    cursor_line = last_line_idx;
    cursor_pos = new_pos;
    cursor_row = new_row;
    cursor_col = new_col;
  }

let go_to_line_start model = { model with cursor_pos = 0; cursor_col = 0 }

let go_to_line_end model =
  let width = effective_width model in
  let new_pos = line_length model in
  let new_row, new_col =
    internal_to_terminal width model.lines (model.cursor_line, new_pos)
  in
  {
    model with
    cursor_pos = new_pos;
    cursor_row = new_row;
    cursor_col = new_col;
  }

let is_word_char s =
  if String.length s > 1 then true
  else
    let c = String.get s 0 in
    Char.Ascii.is_alphanum c

let go_to_next_word model =
  let rec loop seen_word model =
    if at_line_end model then model
    else
      match char_at model with
      | None -> model
      | Some cursor_char ->
          if is_word_char cursor_char then
            let new_mod = move_right model in
            if same_cursor_pos model new_mod then model else loop true new_mod
          else if seen_word then model
          else
            let new_mod = move_right model in
            if same_cursor_pos model new_mod then model else loop false new_mod
  in
  loop false model

let go_to_last_word model =
  let rec loop seen_word model =
    if at_line_start model then model
    else
      match char_before model with
      | None -> model
      | Some cursor_char ->
          if is_word_char cursor_char then
            let new_mod = move_left model in
            if same_cursor_pos model new_mod then model else loop true new_mod
          else if seen_word then model
          else
            let new_mod = move_left model in
            if same_cursor_pos model new_mod then model else loop false new_mod
  in
  loop false model

let shift_history model ~amount =
  let result =
    if amount > 0 then
      History.go_back model.history ~current_prompt:model.lines ()
    else
      History.go_forwards model.history ~current_prompt:model.lines ()
  in
  match result with
  | Some lines ->
      {
        model with
        lines;
        lex_cache = lex_cache_for_lines lines;
        flipping_through_history = Some 2;
      }
      |> move_cursor_to_end
  | None -> model

let delete_before_cursor model =
  let width = effective_width model in
  if at_line_start model then delete_char model
  else
    let line = current_line model in
    let new_line =
      Unicode_string.delete_range line ~start:0 ~len:model.cursor_pos
    in
    let new_lines = update_line model.lines model.cursor_line new_line in
    let new_row, new_col =
      internal_to_terminal width new_lines (model.cursor_line, 0)
    in
    {
      model with
      lines = new_lines;
      cursor_pos = 0;
      cursor_row = new_row;
      cursor_col = new_col;
    }
    |> lexer_update model.cursor_line model.cursor_line

let delete_char_after_cursor model =
  let after_move_right = move_right model in
  if same_cursor_pos model after_move_right then model
  else delete_char after_move_right

let continuation_indent_size = 2

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

type continuation_signal = Submit | Continue_with_indent of int

let token_lexeme_len token = String.length (Syntax.token_to_lexeme token)

let tokens_with_positions tokens =
  let rec loop pos acc = function
    | [] -> List.rev acc
    | tok :: rest ->
        let len = token_lexeme_len tok in
        let next_pos = pos + len in
        loop next_pos ((tok, pos, next_pos) :: acc) rest
  in
  loop 0 [] tokens

let cursor_byte_offset model =
  let line = current_line model in
  if model.cursor_pos >= Unicode_string.length line then
    Unicode_string.byte_length line
  else
    Unicode_string.sub line ~start:0 ~len:model.cursor_pos
    |> Unicode_string.byte_length

let tokens_before_cursor model =
  model.lex_cache
  |> List.filteri (fun i _ -> i <= model.cursor_line)
  |> List.map (fun l -> l.tokens)
  |> List.concat

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
      | R_lexer.LEFT_PAREN -> Some { p with header_depth = p.header_depth + 1 }
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

let mode_at_cursor model =
  let entry = List.nth model.lex_cache model.cursor_line in
  let cursor_byte = cursor_byte_offset model in
  let prefix = String.sub entry.text 0 cursor_byte in
  let _, end_mode = R_lexer.lex_line entry.start_mode prefix in
  end_mode

let inside_empty_brackets model =
  let current_tokens = (List.nth model.lex_cache model.cursor_line).tokens in
  let cursor_byte = cursor_byte_offset model in
  let positioned = tokens_with_positions current_tokens in
  let token_before =
    List.fold_left
      (fun acc (tok, _, end_pos) ->
        if end_pos = cursor_byte then Some tok else acc)
      None positioned
  in
  let token_after =
    List.find_opt (fun (_, start_pos, _) -> start_pos = cursor_byte) positioned
    |> Option.map (fun (tok, _, _) -> tok)
  in
  match (token_before, token_after) with
  | Some R_lexer.LEFT_BRACE, Some R_lexer.RIGHT_BRACE -> true
  | _ -> false

let leading_spaces s =
  let rec loop i =
    if i >= String.length s then i
    else if String.get s i = ' ' then loop (i + 1)
    else i
  in
  loop 0

let insert_spaces count model =
  let rec loop m n = if n <= 0 then m else loop (insert_char m ' ') (n - 1) in
  loop model count

let expand_empty_brackets model =
  let line = current_line model in
  let base_indent = leading_spaces (Unicode_string.to_string line) in
  model |> insert_newline
  |> insert_spaces (base_indent + continuation_indent_size)
  |> insert_newline |> insert_spaces base_indent |> move_up |> go_to_line_end

let at_empty_line model =
  "" = (model |> current_line |> Unicode_string.to_string |> String.trim)

let continuation_signal model =
  if at_empty_line model then Submit
  else
    let tokens = tokens_before_cursor model in
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
      Continue_with_indent indent_levels
    else Submit

let submit model =
  if inside_empty_brackets model then Continue (expand_empty_brackets model)
  else
    match continuation_signal model with
    | Continue_with_indent indent_levels ->
        let line = current_line model in
        let base_indent = leading_spaces (Unicode_string.to_string line) in
        let indent_spaces =
          max base_indent (indent_levels * continuation_indent_size)
        in
        let rec repeat_n_times n f m =
          if n <= 0 then m else repeat_n_times (n - 1) f (f m)
        in
        Continue
          (model |> insert_newline
          |> repeat_n_times indent_spaces (fun m -> insert_char m ' '))
    | Submit ->
        let text =
          String.concat "\n" (List.map Unicode_string.to_string model.lines)
        in
        let width = effective_width model in
        let wrapped = wrap_lines width model.lines in
        let total_rows = List.length wrapped in
        let output_row = model.prompt_top_row + total_rows in
        let new_prompt_top = output_row + 1 in
        let scroll_amount =
          if new_prompt_top > model.term_height then
            model.term_height - new_prompt_top
          else 0
        in
        History.add_to_history model.history model.lines;
        let new_model =
          {
            model with
            awaiting_response = true;
            repl_cursor = (output_row + scroll_amount, 1);
            prompt_top_row = new_prompt_top + scroll_amount;
            previous_prompt_top_row = new_prompt_top + scroll_amount;
            lines = [ Unicode_string.empty ];
            lex_cache = lex_cache_for_lines [ Unicode_string.empty ];
            cursor_row = 0;
            cursor_col = 0;
            cursor_line = 0;
            cursor_pos = 0;
            prompt_box_height = 1;
            scroll_amount;
          }
        in
        Submit (text, new_model)

let handle_vertical_cursor_movement model =
  let width = effective_width model in
  let new_height =
    model.lines |> wrap_lines width |> List.length
    |> max model.prompt_box_height
  in
  let scrolls_from_expansion =
    if new_height > model.prompt_box_height then
      let bottom = model.prompt_top_row + new_height - 1 in
      if bottom > model.term_height then model.term_height - bottom else 0
    else 0
  in
  let prompt_top_after_expansion =
    model.prompt_top_row + scrolls_from_expansion
  in
  let cursor_term_row = model.cursor_row + prompt_top_after_expansion in
  let scrolls_from_cursor_movement =
    if cursor_term_row > model.term_height then
      model.term_height - cursor_term_row
    else if cursor_term_row < 1 then 1 - cursor_term_row
    else 0
  in
  let scrolls = scrolls_from_expansion + scrolls_from_cursor_movement in
  {
    model with
    prompt_box_height = new_height;
    prompt_top_row = model.prompt_top_row + scrolls;
    scroll_amount = scrolls;
  }

let handle_resize new_width model =
  if model.term_width = new_width then model
  else
    let model = { model with term_width = new_width } in
    let new_eff_width = effective_width model in
    let new_row, new_col =
      internal_to_terminal new_eff_width model.lines
        (model.cursor_line, model.cursor_pos)
    in
    { model with cursor_row = new_row; cursor_col = new_col }

let is_empty_input model =
  match model.lines with [ line ] -> Unicode_string.is_empty line | _ -> false

let apply_key key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' when model.awaiting_response -> Cancel
  | Ctrl 'p' when model.awaiting_response -> Continue model
  | Ctrl 'd' ->
      if is_empty_input model then Exit
      else Continue (delete_char_after_cursor model)
  | Enter -> submit model
  | Ctrl 'u' -> Continue (delete_before_cursor model)
  | Ctrl '\r' -> Continue (insert_newline model)
  | Ctrl 'p' -> Continue (shift_history model ~amount:1)
  | Ctrl 'a' -> Continue (go_to_line_start model)
  | Ctrl 'e' -> Continue (go_to_line_end model)
  | Other "next word" -> Continue (go_to_next_word model)
  | Other "last word" -> Continue (go_to_last_word model)
  | Char c -> Continue (user_input_char model c)
  | Backspace -> Continue (user_input_delete model)
  | Left -> Continue (move_left model)
  | Right -> Continue (move_right model)
  | Up ->
      if at_first_line model || Option.is_some model.flipping_through_history
      then Continue (shift_history model ~amount:1)
      else Continue (move_up model)
  | Down ->
      if at_last_line model || Option.is_some model.flipping_through_history
      then Continue (shift_history model ~amount:(-1))
      else Continue (move_down model)
  | Paste text -> Continue (insert_paste model text)
  | _ -> Continue model

let universal_corrections key model =
  model |> handle_vertical_cursor_movement |> fun s ->
  let flipping_through_history =
    match model.flipping_through_history with
    | Some 1 | None -> None
    | Some n -> Some (n - 1)
  in
  { s with previous_key = Some key; flipping_through_history }

let sync_internal_coords model =
  let width = effective_width model in
  let cursor_line, cursor_pos =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  { model with cursor_line; cursor_pos }

let update key model =
  { model with scroll_amount = 0 }
  |> handle_resize model.term_width
  |> sync_internal_coords |> apply_key key
  |> function
  | Continue s -> Continue (universal_corrections key s)
  | other -> other

let process_response model =
  match model.backend_response with
  | None -> failwith "process_response called with no backend_response"
  | Some response ->
      let repl_output =
        match response with
        | Backend.Stdout s -> [ (`Raw, s) ]
        | Backend.Result s -> [ (`Raw, s) ]
        | Backend.R_error s -> [ (`Error, s) ]
        | Backend.Internal_error s -> [ (`Error, "Internal error: " ^ s) ]
        | Backend.Restarted s -> [ (`Error, s) ]
        | Backend.Done -> []
        | Backend.Shutdown -> []
      in
      let awaiting_response =
        match response with
        (* Keep waiting for more output until we get a terminal response *)
        | Backend.Stdout _ | Backend.Result _ | Backend.R_error _ -> true
        (* Terminal responses *)
        | Backend.Done | Backend.Shutdown | Backend.Internal_error _ | Backend.Restarted _ -> false
      in
      {
        model with
        backend_response = None;
        repl_output = Some repl_output;
        awaiting_response;
        scroll_amount = 0;
      }
