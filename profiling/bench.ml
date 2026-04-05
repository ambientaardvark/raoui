open Raoui

module Term = Terminal_ops.Ansi

module V = View.Make (Term)

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
    lex_cache = R_syntax.Cache.create lines;
    theme = Theme.tokyo_night;
    cursor_row = cursor_line;  (* simplified: assume no wrapping *)
    cursor_col = cursor_col;
    cursor_line;
    cursor_pos = cursor_col;
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
    history = History.init "/dev/null";
    flipping_through_history = None;
    running_in_ide = false;
    completion = None;
    mode = Frontend_types.Normal;
  }

let iterations = 10_000

let run_case model key =
  for _ = 1 to iterations do
    let m, _ = Update.update key model in
    let _ = V.view m in
    ()
  done

let () =
  let base = make_model () in
  let line = us "alpha beta gamma" in
  let cases =
    [
      ( base,
        Tty_listener.Char "x" );
      ( { base with cursor_col = 0 },
        Tty_listener.Backspace );
      ( { base with cursor_col = Unicode_string.length line;
                 lines = [ line ];
                 lex_cache = R_syntax.Cache.create [ line ];
                 cursor_row = 0 },
        Tty_listener.Ctrl '\r' );
      ( { base with cursor_col = 0; cursor_row = 0; lines = [ us "abc" ];
                 lex_cache = R_syntax.Cache.create [ us "abc" ] },
        Tty_listener.Right );
      ( { base with lines = [ us "hello world" ]; cursor_col = 5; cursor_row = 0;
                 lex_cache = R_syntax.Cache.create [ us "hello world" ] },
        Tty_listener.Paste "X\nY\nZ" );
    ]
  in
  List.iter (fun (model, key) -> run_case model (Update.Key key)) cases
