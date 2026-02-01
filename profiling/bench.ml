open Raoui

let us s =
  match Unicode_string.of_string s with
  | Ok u -> u
  | Error _ -> failwith "invalid string"

(* 5 lines, cursor on line 2 (middle), ~20 chars, cursor in middle of line *)
let make_model () =
  let lines = [
    us "first line here";
    us "second line content";   (* ~20 chars, cursor here *)
    us "third line of text";
    us "fourth line input";
    us "fifth and final line";
  ] in
  (* Cursor in middle of line 1 (0-indexed) *)
  let cursor_line = 1 in
  let cursor_col = Unicode_string.length (List.nth lines cursor_line) / 2 in
  {
    Frontend_types.
    lines;
    lex_cache = Update.lex_cache_for_lines lines;
    cursor_row = cursor_line;  (* simplified: assume no wrapping *)
    cursor_col = cursor_col;
    prompt_top_row = 1;
    term_width = 80;
    term_height = 24;
    prompt_box_height = 5;
    previous_prompt_top_row = 1;
    previous_key = None;
    persistent_col = cursor_col;
    awaiting_response = false;
    backend_response = None;
    repl_output = None;
    repl_cursor = (10, 1);
    scroll_amount = 0;
    prompt_history = [];
    original_prompt = None;
    place_in_history = 0;
    flipping_through_history = None;
  }

let iterations = 100_000

let () =
  let model = make_model () in
  let key = Tty_listener.Char 'x' in
  for _ = 1 to iterations do
    match Update.update key model with
    | Frontend_types.Continue m ->
        let _ = View.view m in
        ()
    | _ -> ()
  done
