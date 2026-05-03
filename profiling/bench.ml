open Raoui
module Term = Terminal_ops.Ansi
module V = View.Make (Term)

let us s =
  match Unicode_string.of_string s with
  | Ok u -> u
  | Error _ -> failwith "invalid string"

(* 5 lines, cursor on line 2 (middle), ~20 chars, cursor in middle of line *)
let make_model () =
  let lines =
    [
      us "first line here";
      us "second line content";
      (* ~20 chars, cursor here *)
      us "third line of text";
      us "fourth line input";
      us "fifth and final line";
    ]
  in
  (* Cursor in middle of line 1 (0-indexed) *)
  let cursor_line = 1 in
  let cursor_pos = Unicode_string.length (List.nth lines cursor_line) / 2 in
  {
    Frontend_types.input =
      {
        lines;
        lex_cache = R_lex_cache.create lines;
        cursor_line;
        cursor_pos;
        previous_key = None;
        persistent_col = cursor_pos;
        history = History.init "/dev/null";
        flipping_through_history = None;
        completion = None;
        mode = Frontend_types.Normal;
      };
    layout =
      {
        prompt_top_row = 1;
        term_width = 80;
        term_height = 24;
        prompt_box_height = 5;
        previous_prompt_top_row = 1;
        scroll_amount = 0;
        running_in_ide = false;
      };
    repl =
      {
        awaiting_response = false;
        backend_response = None;
        repl_output = None;
        repl_cursor = (10, 1);
      };
    theme = Theme.tokyo_night;
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
      (base, Tty_listener.Char "x");
      ( { base with input = { base.input with cursor_pos = 0 } },
        Tty_listener.Backspace );
      ( {
          base with
          input =
            {
              base.input with
              cursor_pos = Unicode_string.length line;
              cursor_line = 0;
              lines = [ line ];
              lex_cache = R_lex_cache.create [ line ];
            };
        },
        Tty_listener.Ctrl '\r' );
      ( {
          base with
          input =
            {
              base.input with
              cursor_pos = 0;
              cursor_line = 0;
              lines = [ us "abc" ];
              lex_cache = R_lex_cache.create [ us "abc" ];
            };
        },
        Tty_listener.Right );
      ( {
          base with
          input =
            {
              base.input with
              lines = [ us "hello world" ];
              cursor_pos = 5;
              cursor_line = 0;
              lex_cache = R_lex_cache.create [ us "hello world" ];
            };
        },
        Tty_listener.Paste "X\nY\nZ" );
    ]
  in
  List.iter (fun (model, key) -> run_case model (Update.Key key)) cases
