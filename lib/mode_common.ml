open Frontend_types

let submit_aligned_prompt_top model =
  let width = effective_width model in
  let total_rows = model.input.lines |> wrap_lines width |> List.length in
  min model.layout.prompt_top_row (model.layout.term_height - total_rows)

let scroll_terminal_after_submit model =
  let prompt_top_row = submit_aligned_prompt_top model in
  let width = effective_width model in
  let total_rows = wrap_lines width model.input.lines |> List.length in
  (* The submitted lines stay on screen as scrollback; the next output lands on
     [output_row] just below them, and the fresh (one-row) prompt sits below
     that — scrolling up if the trio runs past the bottom of the screen. *)
  let output_row = prompt_top_row + total_rows in
  let new_prompt_top, scroll_amount =
    fit_prompt_below ~term_height:model.layout.term_height ~height:1
      ~anchor_row:(output_row + 1)
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
        prompt_top_row = new_prompt_top;
        previous_prompt_top_row = new_prompt_top;
        prompt_box_height = 1;
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

(* History entries record the input mode they were submitted from as a string
   (the commands.mode column). Readline/History_search never submit through
   the recallable history, so they have no string form. *)
let history_mode_string = function
  | Frontend_types.Normal -> "r"
  | Frontend_types.Shell -> "shell"
  | Frontend_types.Ai -> "ai"
  | Frontend_types.Readline _ | Frontend_types.History_search _ -> assert false

let mode_of_history_string = function
  | "r" -> Frontend_types.Normal
  | "shell" -> Frontend_types.Shell
  | "ai" -> Frontend_types.Ai
  | other -> failwith ("unknown history mode: " ^ other)

(* Recall a history entry into the prompt, switching the input mode to the one
   the entry was submitted from (r/shell/ai). *)
let shift_history model ~amount =
  let current_prompt =
    (history_mode_string model.input.mode, model.input.lines)
  in
  let result =
    if amount > 0 then History.go_back model.input.history ~current_prompt ()
    else History.go_forwards model.input.history ~current_prompt ()
  in
  match result with
  | Some (mode_string, lines) ->
      {
        model with
        input =
          {
            model.input with
            mode = mode_of_history_string mode_string;
            lines;
            lex_cache = R_lex_cache.create lines;
            flipping_through_history = Some 2;
          };
      }
      |> Text_editor.move_cursor_to_end
  | None -> model

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
