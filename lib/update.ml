open Frontend_types

let insert_char model c =
  let width = effective_width model in
  let line_idx, col =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  let line = get_line model.lines line_idx in
  match Unicode_string.insert_string line ~pos:col (String.make 1 c) with
  | Error _ -> model (* Invalid UTF-8 byte, ignore it *)
  | Ok new_line ->
      let new_lines = update_line model.lines line_idx new_line in
      let new_row, new_col =
        internal_to_terminal width new_lines (line_idx, col + 1)
      in
      {
        model with
        lines = new_lines;
        cursor_row = new_row;
        cursor_col = new_col;
      }

let delete_char model =
  let width = effective_width model in
  let line_idx, col =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  if col = 0 then
    if line_idx = 0 then model
    else
      let prev_line = get_line model.lines (line_idx - 1) in
      let curr_line = get_line model.lines line_idx in
      let merged = Unicode_string.append prev_line curr_line in
      let new_lines =
        model.lines
        |> List.filteri (fun i _ -> i <> line_idx)
        |> List.mapi (fun i line -> if i = line_idx - 1 then merged else line)
      in
      let new_row, new_col =
        internal_to_terminal width new_lines
          (line_idx - 1, Unicode_string.length prev_line)
      in
      {
        model with
        lines = new_lines;
        cursor_row = new_row;
        cursor_col = new_col;
      }
  else
    let line = get_line model.lines line_idx in
    let new_line = Unicode_string.delete line (col - 1) in
    let new_lines = update_line model.lines line_idx new_line in
    let new_row, new_col =
      internal_to_terminal width new_lines (line_idx, col - 1)
    in
    { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let insert_newline model =
  let width = effective_width model in
  let line_idx, col =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  let line = get_line model.lines line_idx in
  let before, after = Unicode_string.split line col in
  let new_lines =
    List.concat_map
      (fun (i, l) -> if i = line_idx then [ before; after ] else [ l ])
      (List.mapi (fun i l -> (i, l)) model.lines)
  in
  let new_row, new_col =
    internal_to_terminal width new_lines (line_idx + 1, 0)
  in
  { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let insert_paste model text =
  let max_len = 5 * 1024 in
  let text =
    if String.length text > max_len then String.sub text 0 max_len else text
  in
  let width = effective_width model in
  let line_idx, col =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  let line = get_line model.lines line_idx in
  let before, after = Unicode_string.split line col in
  let paste_lines = String.split_on_char '\n' text in
  let to_us s =
    match Unicode_string.of_string s with
    | Ok u -> u
    | Error _ -> Unicode_string.empty
  in
  let inserted, final_col =
    match paste_lines with
    | [] -> ([ Unicode_string.append before after ], col)
    | [ single ] ->
        let single_us = to_us single in
        ( [ Unicode_string.concat [ before; single_us; after ] ],
          col + Unicode_string.length single_us )
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
      (fun (i, l) -> if i = line_idx then inserted else [ l ])
      (List.mapi (fun i l -> (i, l)) model.lines)
  in
  let final_line_idx = line_idx + List.length paste_lines - 1 in
  let new_row, new_col =
    internal_to_terminal width new_lines (final_line_idx, final_col)
  in
  { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let move_left model =
  let width = effective_width model in
  let line_idx, col =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  if col > 0 then
    let new_row, new_col =
      internal_to_terminal width model.lines (line_idx, col - 1)
    in
    { model with cursor_row = new_row; cursor_col = new_col }
  else if line_idx > 0 then
    let prev_line = get_line model.lines (line_idx - 1) in
    let new_row, new_col =
      internal_to_terminal width model.lines
        (line_idx - 1, Unicode_string.length prev_line)
    in
    { model with cursor_row = new_row; cursor_col = new_col }
  else model

let move_right model =
  let width = effective_width model in
  let line_idx, col =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  let line = get_line model.lines line_idx in
  if col < Unicode_string.length line then
    let new_row, new_col =
      internal_to_terminal width model.lines (line_idx, col + 1)
    in
    { model with cursor_row = new_row; cursor_col = new_col }
  else if line_idx < List.length model.lines - 1 then
    let new_row, new_col =
      internal_to_terminal width model.lines (line_idx + 1, 0)
    in
    { model with cursor_row = new_row; cursor_col = new_col }
  else model

let move_up model =
  let wrapped_lines = wrap_lines (effective_width model) model.lines in
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
    {
      model with
      cursor_row = model.cursor_row - 1;
      cursor_col = min target_col line_width;
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
    {
      model with
      cursor_row = model.cursor_row + 1;
      cursor_col = min target_col line_width;
      persistent_col = target_col;
    }
  else model

let move_cursor_to_end model =
  let width = effective_width model in
  let last_line_idx = List.length model.lines - 1 in
  let last_line = List.nth model.lines last_line_idx in
  let new_row, new_col =
    internal_to_terminal width model.lines
      (last_line_idx, Unicode_string.length last_line)
  in
  { model with cursor_row = new_row; cursor_col = new_col }

let shift_history model ~amount =
  let original_prompt =
    if model.place_in_history = 0 then model.lines
    else Option.get model.original_prompt
  in
  let update_to lines place =
    {
      model with
      lines;
      place_in_history = place;
      flipping_through_history = Some 2;
      original_prompt = Some original_prompt;
    }
    |> move_cursor_to_end
  in
  let destination = model.place_in_history + amount in
  match destination with
  | d when d < 0 && model.place_in_history = 0 -> model
  | d when d < 0 -> update_to original_prompt 0
  | d -> (
      match List.nth_opt (original_prompt :: model.prompt_history) d with
      | None -> model
      | Some new_prompt_text -> update_to new_prompt_text d)

let delete_before_cursor model =
  let width = effective_width model in
  let line_idx, col =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  if col = 0 then delete_char model
  else
    let line = List.nth model.lines line_idx in
    let new_line = Unicode_string.delete_range line ~start:0 ~len:col in
    let new_lines = update_line model.lines line_idx new_line in
    let new_row, new_col = internal_to_terminal width new_lines (line_idx, 0) in
    { model with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let delete_char_after_cursor model =
  let after_move_right = move_right model in
  if
    model.cursor_row = after_move_right.cursor_row
    && model.cursor_col = after_move_right.cursor_col
  then model
  else delete_char after_move_right

let submit model =
  (* Check if expression needs continuation *)
  let lines_as_strings = List.map Unicode_string.to_string model.lines in
  let cont = Syntax.check_lines lines_as_strings in
  if cont.needs_continuation then
    (* Insert newline instead of submitting *)
    Continue (insert_newline model)
  else
    let text = String.concat "\n" lines_as_strings in
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
    let new_model =
      {
        model with
        awaiting_response = true;
        repl_cursor = (output_row + scroll_amount, 1);
        prompt_top_row = new_prompt_top + scroll_amount;
        previous_prompt_top_row = new_prompt_top + scroll_amount;
        lines = [ Unicode_string.empty ];
        cursor_row = 0;
        cursor_col = 0;
        prompt_box_height = 1;
        prompt_history = model.lines :: model.prompt_history;
        original_prompt = None;
        place_in_history = 0;
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
    if
      new_height > model.prompt_box_height
      && model.term_height < new_height + model.prompt_top_row - 1
    then -1
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
    let old_eff_width = effective_width model in
    let line_idx, col =
      terminal_to_internal old_eff_width model.lines
        (model.cursor_row, model.cursor_col)
    in
    let model = { model with term_width = new_width } in
    let new_eff_width = effective_width model in
    let new_row, new_col =
      internal_to_terminal new_eff_width model.lines (line_idx, col)
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
  | Char c -> Continue (insert_char model c)
  | Backspace -> Continue (delete_char model)
  | Left -> Continue (move_left model)
  | Right -> Continue (move_right model)
  | Up ->
      let width = effective_width model in
      let row, _col =
        terminal_to_internal width model.lines
          (model.cursor_row, model.cursor_col)
      in
      let at_top = row = 0 in
      if at_top || Option.is_some model.flipping_through_history then
        Continue (shift_history model ~amount:1)
      else Continue (move_up model)
  | Down ->
      let width = effective_width model in
      let wrapped = wrap_lines width model.lines in
      let row, _col =
        terminal_to_internal width model.lines
          (model.cursor_row, model.cursor_col)
      in
      let at_bottom = row = List.length wrapped - 1 in
      if at_bottom || Option.is_some model.flipping_through_history then
        Continue (shift_history model ~amount:(-1))
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

let update key model =
  { model with scroll_amount = 0 }
  |> handle_resize model.term_width
  |> apply_key key
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
        | Backend.Done -> []
        | Backend.Shutdown -> []
      in
      let awaiting_response =
        match response with
        (* Keep waiting for more output until we get a terminal response *)
        | Backend.Stdout _ | Backend.Result _ | Backend.R_error _ -> true
        (* Terminal responses *)
        | Backend.Done | Backend.Shutdown | Backend.Internal_error _ -> false
      in
      {
        model with
        backend_response = None;
        repl_output = Some repl_output;
        awaiting_response;
        scroll_amount = 0;
      }
