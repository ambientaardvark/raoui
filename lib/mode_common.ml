open Frontend_types

let scroll_terminal_after_submit model =
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
  (output_row, scroll_amount, new_prompt_top)

let clear_model_for_submit ?(awaiting_response = true) model =
  let output_row, scroll_amount, new_prompt_top =
    scroll_terminal_after_submit model
  in
  {
    model with
    awaiting_response;
    completion = None;
    repl_cursor = (output_row + scroll_amount, 1);
    prompt_top_row = new_prompt_top + scroll_amount;
    previous_prompt_top_row = new_prompt_top + scroll_amount;
    lines = [ Unicode_string.empty ];
    lex_cache = R_lex_cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
    prompt_box_height = min_prompt_height;
    scroll_amount;
  }

let set_mode_normal_blank model =
  {
    model with
    mode = Frontend_types.Normal;
    lines = [ Unicode_string.empty ];
    lex_cache = R_lex_cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
  }
