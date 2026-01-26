open Frontend_types

let insert_char model c =
  let width = effective_width model in
  let (line_idx, col) = terminal_to_internal width model.lines (model.cursor_row, model.cursor_col) in
  let line = get_line model.lines line_idx in
  let before = String.sub line 0 col in
  let after = String.sub line col (String.length line - col) in
  let new_line = before ^ String.make 1 c ^ after in
  let new_lines = update_line model.lines line_idx new_line in
  let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, col + 1) in
  { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let delete_char model =
  let width = effective_width model in
  let (line_idx, col) = terminal_to_internal width model.lines (model.cursor_row, model.cursor_col) in
  if col = 0 then
    if line_idx = 0 then model
    else
      let prev_line = get_line model.lines (line_idx - 1) in
      let curr_line = get_line model.lines line_idx in
      let merged = prev_line ^ curr_line in
      let new_lines = model.lines
        |> List.filteri (fun i _ -> i <> line_idx)
        |> List.mapi (fun i line -> if i = line_idx - 1 then merged else line)
      in
      let (new_row, new_col) = internal_to_terminal width new_lines (line_idx - 1, String.length prev_line) in
      { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }
  else
    let line = get_line model.lines line_idx in
    let before = String.sub line 0 (col - 1) in
    let after = String.sub line col (String.length line - col) in
    let new_lines = update_line model.lines line_idx (before ^ after) in
    let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, col - 1) in
    { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let insert_newline model =
  let width = effective_width model in
  let (line_idx, col) = terminal_to_internal width model.lines (model.cursor_row, model.cursor_col) in
  let line = get_line model.lines line_idx in
  let before = String.sub line 0 col in
  let after = String.sub line col (String.length line - col) in
  let new_lines =
    List.concat_map (fun (i, l) ->
      if i = line_idx then [before; after] else [l]
    ) (List.mapi (fun i l -> (i, l)) model.lines)
  in
  let (new_row, new_col) = internal_to_terminal width new_lines (line_idx + 1, 0) in
  { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let move_left model =
  let width = effective_width model in
  let wrapped_lines = wrap_lines width model.lines in
  if model.cursor_col > 0 then
    { model with cursor_col = model.cursor_col - 1 }
  else if model.cursor_row > 0 then
    { model with
      cursor_row = model.cursor_row - 1;
      cursor_col = (List.nth wrapped_lines (model.cursor_row - 1) |> String.length)
    }
  else
    model

let move_right model =
  let width = effective_width model in
  let total_rows = List.length (wrap_lines width model.lines) in
  let (line_idx, col) = terminal_to_internal width model.lines (model.cursor_row, model.cursor_col) in
  let line = get_line model.lines line_idx in
  if col >= String.length line then
    if line_idx < List.length model.lines - 1 then
      let (new_row, new_col) = internal_to_terminal width model.lines (line_idx + 1, 0) in
      { model with cursor_row = new_row; cursor_col = new_col }
    else
      model
  else if model.cursor_col < width - 1 then
    { model with cursor_col = model.cursor_col + 1 }
  else if model.cursor_row < total_rows - 1 then
    { model with cursor_row = model.cursor_row + 1; cursor_col = 0 }
  else
    model

let move_up model =
  let wrapped_lines = wrap_lines (effective_width model) model.lines in
  if model.cursor_row > 0 then
    let target_col = match model.previous_key with
      | Some Up | Some Down -> model.persistent_col
      | _ -> model.cursor_col
    in
    let line_length = List.nth wrapped_lines (model.cursor_row - 1) |> String.length in
    { model with
      cursor_row = model.cursor_row - 1;
      cursor_col = min target_col line_length;
      persistent_col = target_col;
    }
  else
    model

let move_down model =
  let width = effective_width model in
  let wrapped_lines = wrap_lines width model.lines in
  let total_rows = List.length wrapped_lines in
  if model.cursor_row < total_rows - 1 then
    let target_col = match model.previous_key with
      | Some Up | Some Down -> model.persistent_col
      | _ -> model.cursor_col
    in
    let line_length = List.nth wrapped_lines (model.cursor_row + 1) |> String.length in
    { model with
      cursor_row = model.cursor_row + 1;
      cursor_col = min target_col line_length;
      persistent_col = target_col;
    }
  else
    model

let delete_before_cursor model =
  let width = effective_width model in
  let (line_idx, col) = terminal_to_internal width model.lines (model.cursor_row, model.cursor_col) in
  if col = 0 then
    delete_char model
  else
    let line_str = List.nth model.lines line_idx in
    let new_line_str = String.sub line_str col (String.length line_str - col) in
    let new_lines = update_line model.lines line_idx new_line_str in
    let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, 0) in
    { model with
      lines = new_lines;
      cursor_row = new_row;
      cursor_col = new_col;
    }

let delete_char_after_cursor model =
  let after_move_right = move_right model in
  if model.cursor_row = after_move_right.cursor_row && model.cursor_col = after_move_right.cursor_col
  then model
  else delete_char after_move_right

let submit model =
  let text = String.concat "\n" model.lines in
  let width = effective_width model in
  let wrapped = wrap_lines width model.lines in
  let total_rows = List.length wrapped in
  let output_row = model.prompt_top_row + total_rows in
  let new_prompt_top = output_row + 1 in
  let scroll_amount =
    if new_prompt_top > model.term_height
    then model.term_height - new_prompt_top
    else 0
  in
  let new_model =
    { model with
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
  Submit (text, new_model)

let handle_vertical_cursor_movement model =
  let width = effective_width model in
  let new_height =
    model.lines
    |> wrap_lines width
    |> List.length
    |> max model.prompt_box_height
  in
  let scrolls_from_expansion =
    if new_height > model.prompt_box_height && model.term_height < new_height + model.prompt_top_row - 1
    then (-1)
    else 0
  in
  let prompt_top_after_expansion = model.prompt_top_row + scrolls_from_expansion in
  let cursor_term_row = model.cursor_row + prompt_top_after_expansion in
  let scrolls_from_cursor_movement =
    if cursor_term_row > model.term_height
    then model.term_height - cursor_term_row
    else if cursor_term_row < 1
    then 1 - cursor_term_row
    else 0
  in
  let scrolls = scrolls_from_expansion + scrolls_from_cursor_movement in
  { model with
    prompt_box_height = new_height;
    prompt_top_row = model.prompt_top_row + scrolls;
    scroll_amount = scrolls;
  }

let handle_resize new_width model =
  if model.term_width = new_width then model
  else
    let old_eff_width = effective_width model in
    let (line_idx, col) = terminal_to_internal old_eff_width model.lines (model.cursor_row, model.cursor_col) in
    let model = { model with term_width = new_width } in
    let new_eff_width = effective_width model in
    let (new_row, new_col) = internal_to_terminal new_eff_width model.lines (line_idx, col) in
    { model with cursor_row = new_row; cursor_col = new_col }

let apply_key key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' when model.awaiting_response -> Cancel
  | Ctrl 'p' when model.awaiting_response -> Continue model
  | Ctrl 'd' ->
    if model.lines = [""]
    then Exit
    else Continue (delete_char_after_cursor model)
  | Ctrl 'p' -> submit model
  | Ctrl 'u' -> Continue (delete_before_cursor model)
  | Enter -> Continue (insert_newline model)
  | Char c -> Continue (insert_char model c)
  | Backspace -> Continue (delete_char model)
  | Left -> Continue (move_left model)
  | Right -> Continue (move_right model)
  | Up -> Continue (move_up model)
  | Down -> Continue (move_down model)
  | _ -> Continue model

let universal_corrections key model =
  model
  |> handle_vertical_cursor_movement
  |> (fun s -> { s with previous_key = Some key })

let update key ~term_width model =
  { model with scroll_amount = 0 }
  |> handle_resize term_width
  |> apply_key key
  |> function
    | Continue s -> Continue (universal_corrections key s)
    | other -> other

let process_response model =
  match model.backend_response with
  | None -> failwith "process_response called with no backend_response"
  | Some response ->
    let repl_output = match response with
      | Backend.Stdout s -> [(`Raw, s)]
      | Backend.Result s -> [(`Raw, s)]
      | Backend.R_error s -> [(`Error, s)]
      | Backend.Internal_error s -> [(`Error, "Internal error: " ^ s)]
      | Backend.Done -> []
      | Backend.Shutdown -> []
    in
    let awaiting_response = match response with
      (* Keep waiting for more output until we get a terminal response *)
      | Backend.Stdout _ | Backend.Result _ | Backend.R_error _ -> true
      (* Terminal responses *)
      | Backend.Done | Backend.Shutdown | Backend.Internal_error _ -> false
    in
    { model with
      backend_response = None
    ; repl_output = Some repl_output
    ; awaiting_response
    ; scroll_amount = 0
    }
