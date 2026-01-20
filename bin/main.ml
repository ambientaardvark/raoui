open Raoui
open Frontend_types

let set_raw_mode () =
  let termio = Unix.tcgetattr Unix.stdin in
  let raw_termio = { termio with
    Unix.c_icanon = false;
    Unix.c_echo = false;
    Unix.c_isig = false;
    Unix.c_vmin = 1;
    Unix.c_vtime = 0;
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_termio;
  termio

let restore_mode termio =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

let set_solid_cursor () = print_string "\x1b[2 q"; flush stdout

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
    previous_prompt_top_row = row; previous_key = None; persistent_col = 0;
    awaiting_response = false; backend_response = None; repl_output = None;
    repl_cursor = (row, 1); scroll_amount = 0 }

let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col

let print_repl_output state =
  match state.repl_output with
  | None -> state
  | Some text ->
    let (row, col) = state.repl_cursor in
    print_string (cursor_to row col);
    print_string text;
    flush stdout;
    let (new_row, _) = get_cursor_position () in
    { state with
      repl_output = None;
      repl_cursor = (new_row, 1);
      prompt_top_row = max state.prompt_top_row (new_row + 1);
      lines = [""];
      cursor_row = 0;
      cursor_col = 0;
      prompt_box_height = 1;
    }

let run env =
  let clock = Eio.Stdenv.clock env in
  let stdin = Eio.Stdenv.stdin env in
  let backend = Backend.create clock in
  let rec loop state pending =
    print_string (View.view state);
    flush stdout;
    let (term_width, _) = get_term_dimensions () in

    if pending then
      match Eio.Fiber.first
        (fun () -> `Key (Tty_listener.await_input ~clock ~stdin))
        (fun () -> `Response (Backend.await_response backend))
      with
      | `Key key -> (
        match Update.update key ~term_width state with
        | Frontend_types.Cancel ->
          Backend.cancel backend;
          flush stdout;
          loop (make_init ()) false
        | Frontend_types.Continue new_state ->
          loop new_state true
        | Frontend_types.Exit | Frontend_types.Submit _ ->
          loop state true)
      | `Response r ->
        let new_state =
          { state with backend_response = Some r }
          |> Update.process_response
          |> print_repl_output
        in
        loop new_state new_state.awaiting_response
    else
      let key = Tty_listener.await_input ~clock ~stdin in
      match Update.update key ~term_width state with
      | Frontend_types.Exit -> ()
      | Frontend_types.Submit (text, new_state) ->
        Backend.submit backend text;
        loop new_state true
      | Frontend_types.Continue new_state ->
        loop new_state false
      | Frontend_types.Cancel ->
        loop state false
  in
  loop (make_init ()) false

let () =
  clear_log ();
  Eio_main.run @@ fun env ->
    let orig = set_raw_mode () in
    set_solid_cursor ();
    Fun.protect 
      ~finally:(fun () -> print_newline (); restore_mode orig) 
      (fun () -> run env)
