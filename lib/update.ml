open Frontend_types

let insert_char state c =
  let width = effective_width state in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  let line = get_line state.lines line_idx in
  let before = String.sub line 0 col in
  let after = String.sub line col (String.length line - col) in
  let new_line = before ^ String.make 1 c ^ after in
  let new_lines = update_line state.lines line_idx new_line in
  let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, col + 1) in
  { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let delete_char state =
  let width = effective_width state in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  if col = 0 then
    if line_idx = 0 then state
    else
      let prev_line = get_line state.lines (line_idx - 1) in
      let curr_line = get_line state.lines line_idx in
      let merged = prev_line ^ curr_line in
      let new_lines = state.lines
        |> List.filteri (fun i _ -> i <> line_idx)
        |> List.mapi (fun i line -> if i = line_idx - 1 then merged else line)
      in
      let (new_row, new_col) = internal_to_terminal width new_lines (line_idx - 1, String.length prev_line) in
      { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }
  else
    let line = get_line state.lines line_idx in
    let before = String.sub line 0 (col - 1) in
    let after = String.sub line col (String.length line - col) in
    let new_lines = update_line state.lines line_idx (before ^ after) in
    let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, col - 1) in
    { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let insert_newline state =
  let width = effective_width state in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  let line = get_line state.lines line_idx in
  let before = String.sub line 0 col in
  let after = String.sub line col (String.length line - col) in
  let new_lines =
    List.concat_map (fun (i, l) ->
      if i = line_idx then [before; after] else [l]
    ) (List.mapi (fun i l -> (i, l)) state.lines)
  in
  let (new_row, new_col) = internal_to_terminal width new_lines (line_idx + 1, 0) in
  { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let move_left state =
  let width = effective_width state in
  let wrapped_lines = wrap_lines width state.lines in
  if state.cursor_col > 0 then
    { state with cursor_col = state.cursor_col - 1 }
  else if state.cursor_row > 0 then
    { state with
      cursor_row = state.cursor_row - 1;
      cursor_col = (List.nth wrapped_lines (state.cursor_row - 1) |> String.length)
    }
  else
    state

let move_right state =
  let width = effective_width state in
  let total_rows = List.length (wrap_lines width state.lines) in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  let line = get_line state.lines line_idx in
  if col >= String.length line then
    if line_idx < List.length state.lines - 1 then
      let (new_row, new_col) = internal_to_terminal width state.lines (line_idx + 1, 0) in
      { state with cursor_row = new_row; cursor_col = new_col }
    else
      state
  else if state.cursor_col < width - 1 then
    { state with cursor_col = state.cursor_col + 1 }
  else if state.cursor_row < total_rows - 1 then
    { state with cursor_row = state.cursor_row + 1; cursor_col = 0 }
  else
    state

let move_up state =
  let wrapped_lines = wrap_lines (effective_width state) state.lines in
  if state.cursor_row > 0 then
    let target_col = match state.previous_key with
      | Some Up | Some Down -> state.persistent_col
      | _ -> state.cursor_col
    in
    let line_length = List.nth wrapped_lines (state.cursor_row - 1) |> String.length in
    { state with
      cursor_row = state.cursor_row - 1;
      cursor_col = min target_col line_length;
      persistent_col = target_col;
    }
  else
    state

let move_down state =
  let width = effective_width state in
  let wrapped_lines = wrap_lines width state.lines in
  let total_rows = List.length wrapped_lines in
  if state.cursor_row < total_rows - 1 then
    let target_col = match state.previous_key with
      | Some Up | Some Down -> state.persistent_col
      | _ -> state.cursor_col
    in
    let line_length = List.nth wrapped_lines (state.cursor_row + 1) |> String.length in
    { state with
      cursor_row = state.cursor_row + 1;
      cursor_col = min target_col line_length;
      persistent_col = target_col;
    }
  else
    state

let delete_before_cursor state =
  let width = effective_width state in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  if col = 0 then
    delete_char state
  else
    let line_str = List.nth state.lines line_idx in
    let new_line_str = String.sub line_str col (String.length line_str - col) in
    let new_lines = update_line state.lines line_idx new_line_str in
    let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, 0) in
    { state with
      lines = new_lines;
      cursor_row = new_row;
      cursor_col = new_col;
    }

let delete_char_after_cursor state = 
  let after_move_right = move_right state in
  if state.cursor_row = after_move_right.cursor_row && state.cursor_col = after_move_right.cursor_col 
  then state
  else delete_char after_move_right

let submit state =
  let text = String.concat "\n" state.lines in
  let width = effective_width state in
  let wrapped = wrap_lines width state.lines in
  let total_rows = List.length wrapped in
  let output_row = state.prompt_top_row + total_rows in
  let new_prompt_top = output_row + 1 in
  let scroll_amount =
    if new_prompt_top > state.term_height
    then state.term_height - new_prompt_top
    else 0
  in
  let new_state =
    { state with
      awaiting_response = true
    ; repl_cursor = (output_row + scroll_amount, 1)
    ; prompt_top_row = new_prompt_top + scroll_amount
    ; previous_prompt_top_row = new_prompt_top + scroll_amount
    ; lines = [""]
    ; cursor_row = 0
    ; cursor_col = 0
    ; prompt_box_height = 1
    ; scroll_amount
    }
  in
  Submit (text, new_state)

let handle_vertical_cursor_movement state =
  let width = effective_width state in
  let new_height =
    state.lines
    |> wrap_lines width
    |> List.length
    |> max state.prompt_box_height
  in
  let scrolls_from_expansion =
    if new_height > state.prompt_box_height && state.term_height < new_height + state.prompt_top_row - 1
    then (-1)
    else 0
  in
  let prompt_top_after_expansion = state.prompt_top_row + scrolls_from_expansion in
  let cursor_term_row = state.cursor_row + prompt_top_after_expansion in
  let scrolls_from_cursor_movement =
    if cursor_term_row > state.term_height
    then state.term_height - cursor_term_row
    else if cursor_term_row < 1
    then 1 - cursor_term_row
    else 0
  in
  let scrolls = scrolls_from_expansion + scrolls_from_cursor_movement in
  { state with
    prompt_box_height = new_height;
    prompt_top_row = state.prompt_top_row + scrolls;
    scroll_amount = scrolls;
  }

let handle_resize new_width state =
  if state.term_width = new_width then state
  else
    let old_eff_width = effective_width state in
    let (line_idx, col) = terminal_to_internal old_eff_width state.lines (state.cursor_row, state.cursor_col) in
    let state = { state with term_width = new_width } in
    let new_eff_width = effective_width state in
    let (new_row, new_col) = internal_to_terminal new_eff_width state.lines (line_idx, col) in
    { state with cursor_row = new_row; cursor_col = new_col }

let apply_key key state =
  let open Tty_listener in
  if state.awaiting_response then
    match key with
    | Ctrl 'c' -> Cancel
    | _ -> Continue state
  else
    match key with
    | Ctrl 'd' ->
      if state.lines = [""]
      then Exit
      else Continue (delete_char_after_cursor state)
    | Ctrl 'p' -> submit state
    | Ctrl 'u' -> Continue (delete_before_cursor state)
    | Enter -> Continue (insert_newline state)
    | Char c -> Continue (insert_char state c)
    | Backspace -> Continue (delete_char state)
    | Left -> Continue (move_left state)
    | Right -> Continue (move_right state)
    | Up -> Continue (move_up state)
    | Down -> Continue (move_down state)
    | _ -> Continue state

let universal_corrections key state =
  state
  |> handle_vertical_cursor_movement
  |> (fun s -> { s with previous_key = Some key })

let update key ~term_width state =
  { state with scroll_amount = 0 }
  |> handle_resize term_width
  |> apply_key key
  |> function
    | Continue s -> Continue (universal_corrections key s)
    | other -> other

let process_response state =
  match state.backend_response with
  | None -> failwith "process_response called with no backend_response"
  | Some response ->
    let repl_output = match response with
      | Backend.Complete s -> s
      | Backend.Partial s -> s
      | Backend.Error s -> "Error: " ^ s
    in
    let awaiting_response = match response with
      | Backend.Partial _ -> true
      | Backend.Complete _ | Backend.Error _ -> false
    in
    { state with
      backend_response = None
    ; repl_output = Some repl_output
    ; awaiting_response
    ; scroll_amount = 0
    }
