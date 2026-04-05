open Frontend_types

let lexer_update start_line end_line model =
  match model.mode with
  | Normal | History_search _ ->
      {
        model with
        lex_cache =
          R_lex_cache.update ~start_line ~end_line ~lines:model.lines
            model.lex_cache;
      }
  | Shell | Readline _ ->
      { model with lex_cache = R_lex_cache.make_all_default model.lines }

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

let should_auto_pair model =
  match char_at model with
  | None -> true
  | Some s -> (
      String.length s = 1
      &&
      match String.get s 0 with
      | ')' | ']' | '}' | ' ' | '\t' -> true
      | _ -> false)

let at_first_line model = model.cursor_line = 0
let at_last_line model = model.cursor_line >= List.length model.lines - 1

let same_cursor_pos m1 m2 =
  m1.cursor_line = m2.cursor_line && m1.cursor_pos = m2.cursor_pos

let prompt_is_empty model =
  model.lines = [] || model.lines = [ Unicode_string.empty ]

let insert_char model s =
  let width = effective_width model in
  let line = current_line model in
  match Unicode_string.insert_string line ~pos:model.cursor_pos s with
  | Error _ -> model
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
      | Some Tty_listener.Up | Some Tty_listener.Down -> model.persistent_col
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
      | Some Tty_listener.Up | Some Tty_listener.Down -> model.persistent_col
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

let user_input_char model c =
  match c with
  | "[" | "{" | "(" -> (
      if not (should_auto_pair model) then insert_char model c
      else
        let after1 = insert_char model c in
        match c with
        | "[" -> move_left (insert_char after1 "]")
        | "(" -> move_left (insert_char after1 ")")
        | "{" -> move_left (insert_char after1 "}")
        | _ -> assert false)
  | "]" | "}" | ")" -> (
      if at_line_end model then insert_char model c
      else
        match char_at model with
        | Some s when s = c -> move_right model
        | _ -> insert_char model c)
  | "'" | "\"" -> (
      match char_at model with
      | Some s when s = c -> move_right model
      | _ ->
          if should_auto_pair model then
            insert_char (insert_char model c) c |> move_left
          else insert_char model c)
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
    match String.get s 0 with
    | ' ' | '\t' | '/' | ',' | '=' | '-' | '+' | '[' | ']' | '{' | '}' | '('
    | ')' | '|' | '\\' | '?' | '<' | '>' | '`' | '~' | '!' | '@' | '#' | '$'
    | '%' | '^' | '&' | '*' | ';' | ':' | '\'' | '"' ->
        false
    | _ -> true

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

let is_empty_input model =
  match model.lines with [ line ] -> Unicode_string.is_empty line | _ -> false

let replace_range model start_pos end_pos text =
  let line = current_line model in
  let start_pos = max 0 start_pos in
  let end_pos = max start_pos end_pos in
  let before = Unicode_string.sub line ~start:0 ~len:start_pos in
  let after_start = min end_pos (Unicode_string.length line) in
  let after_len = Unicode_string.length line - after_start in
  let after = Unicode_string.sub line ~start:after_start ~len:after_len in
  let text_us =
    match Unicode_string.of_string text with
    | Ok u -> u
    | Error _ -> Unicode_string.empty
  in
  let new_line = Unicode_string.concat [ before; text_us; after ] in
  let new_lines = update_line model.lines model.cursor_line new_line in
  let new_cursor_pos = start_pos + Unicode_string.length text_us in
  let model = { model with lines = new_lines; cursor_pos = new_cursor_pos } in
  let model = lexer_update model.cursor_line model.cursor_line model in
  let width = effective_width model in
  let new_row, new_col =
    internal_to_terminal width model.lines (model.cursor_line, new_cursor_pos)
  in
  { model with cursor_row = new_row; cursor_col = new_col }

let replace_token model token_start text =
  replace_range model token_start model.cursor_pos text
