open Frontend_types

let submit_aligned_prompt_top model =
  let width = effective_width model in
  let total_rows = model.input.lines |> wrap_lines width |> List.length in
  min model.layout.prompt_top_row (model.layout.term_height - total_rows)

let scroll_terminal_after_submit model =
  let prompt_top_row = submit_aligned_prompt_top model in
  let width = effective_width model in
  let wrapped = wrap_lines width model.input.lines in
  let total_rows = List.length wrapped in
  let output_row = prompt_top_row + total_rows in
  let new_prompt_top = output_row + 1 in
  let scroll_amount =
    if new_prompt_top > model.layout.term_height then
      model.layout.term_height - new_prompt_top
    else 0
  in
  (output_row, scroll_amount, new_prompt_top)

let clear_model_for_submit ?(awaiting_response = true) model =
  let output_row, scroll_amount, new_prompt_top =
    scroll_terminal_after_submit model
  in
  {
    model with
    repl =
      {
        model.repl with
        awaiting_response;
        repl_cursor = (output_row + scroll_amount, 1);
      };
    layout =
      {
        model.layout with
        prompt_top_row = new_prompt_top + scroll_amount;
        previous_prompt_top_row = new_prompt_top + scroll_amount;
        prompt_box_height = min_prompt_height;
        scroll_amount;
      };
    input =
      {
        model.input with
        completion = None;
        lines = [ Unicode_string.empty ];
        lex_cache = R_lex_cache.create [ Unicode_string.empty ];
        cursor_line = 0;
        cursor_pos = 0;
      };
  }

let set_mode_normal_blank model =
  {
    model with
    input =
      {
        model.input with
        mode = Frontend_types.Normal;
        lines = [ Unicode_string.empty ];
        lex_cache = R_lex_cache.create [ Unicode_string.empty ];
        cursor_line = 0;
        cursor_pos = 0;
      };
  }
