open Raoui

let set_raw_mode () =
  let termio = Unix.tcgetattr Unix.stdin in
  let raw_termio = { termio with
    Unix.c_icanon = false;
    Unix.c_echo = false;
    Unix.c_vmin = 1;
    Unix.c_vtime = 0;
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_termio;
  termio

let restore_mode termio =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

let clear_log () =
  let oc = open_out "debug_log.txt" in
  Printf.fprintf oc "";
  close_out oc

let get_cursor_position () =
  print_string "\x1b[6n";
  flush stdout;
  let buf = Buffer.create 16 in
  let rec read_until_r () =
    let c = input_char stdin in
    if c = 'R' then ()
    else (Buffer.add_char buf c; read_until_r ())
  in
  read_until_r ();
  let response = Buffer.contents buf in
  Scanf.sscanf response "\x1b[%d;%d" (fun row col -> (row, col))

let get_term_dimensions () =
  let height = match Terminal_size.get_rows () with
    | Some h -> h
    | None -> failwith "can't get terminal height"
  in
  let width = match Terminal_size.get_columns () with
    | Some w -> w
    | None -> failwith "can't get terminal width"
  in
  (width, height)

let make_init () : Frontend_types.state =
  let (row, _col) = get_cursor_position () in
  let (term_width, term_height) = get_term_dimensions () in
  { lines = [""]; cursor_row = 0; cursor_col = 0;
    prompt_top_row = row; term_width; term_height; prompt_box_height = 1;
    previous_prompt_top_row = row; previous_key = None; persistent_col = 0 }

let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col

let receive_response state text =
  let width = Frontend_types.effective_width state in
  let wrapped = Frontend_types.wrap_lines width state.lines in
  let total_rows = List.length wrapped in
  let output_row = state.prompt_top_row + total_rows - 1 in
  print_string (cursor_to output_row 1);
  Printf.printf "\n%s\n" text;
  flush stdout;
  make_init ()

let run () =
  let rec loop state =
    print_string (View.view state);
    flush stdout;
    let (term_width, _) = get_term_dimensions () in
    let key = Tty_listener.await_input () in
    match Update.update key ~term_width state with
    | Frontend_types.Exit -> ()
    | Frontend_types.Submit text ->
      let new_state = receive_response state text in
      loop new_state
    | Frontend_types.Continue new_state ->
      loop new_state
  in
  loop (make_init ())

let () =
  clear_log ();
  let orig = set_raw_mode () in
  Fun.protect ~finally:(fun () -> print_newline (); restore_mode orig) run
