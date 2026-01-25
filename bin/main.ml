open Base
open Stdio
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

let set_solid_cursor () =
  print_string "\x1b[2 q";
  Out_channel.flush stdout

let clear_log () =
  Out_channel.write_all "debug_log.txt" ~data:""

let get_cursor_position () =
  print_string "\x1b[6n";
  Out_channel.flush stdout;
  let buf = Buffer.create 16 in
  let rec read_until_r () =
    let c = In_channel.(input_char stdin) |> Option.value_exn in
    if Char.(c = 'R') then ()
    else (Buffer.add_char buf c; read_until_r ())
  in
  read_until_r ();
  let response = Buffer.contents buf in
  Stdlib.Scanf.sscanf response "\x1b[%d;%d" (fun row col -> (row, col))

let get_term_dimensions () =
  let height =
    Terminal_size.get_rows ()
    |> Option.value_exn ~message:"can't get terminal height"
  in
  let width =
    Terminal_size.get_columns ()
    |> Option.value_exn ~message:"can't get terminal width"
  in
  (width, height)

let make_init () : Frontend_types.model =
  let (row, _col) = get_cursor_position () in
  let (term_width, term_height) = get_term_dimensions () in
  { lines = [""]; cursor_row = 0; cursor_col = 0;
    prompt_top_row = row; term_width; term_height; prompt_box_height = 1;
    previous_prompt_top_row = row; previous_key = None; persistent_col = 0;
    awaiting_response = false; backend_response = None; repl_output = None;
    repl_cursor = (row, 1); scroll_amount = 0 }

let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col

(* NOTE: This clears from repl_cursor to end of screen before printing output,
   which erases any prompt the user typed while waiting. View then repaints.
   This may not work correctly with output that uses cursor movement (e.g.
   progress bars with \r) - get_cursor_position won't reflect actual extent. *)
let print_repl_output model =
  match model.repl_output with
  | None | Some "" -> { model with repl_output = None }
  | Some text ->
    let (row, col) = model.repl_cursor in
    print_string (cursor_to row col);
    print_string "\x1b[J";
    print_string text;
    Out_channel.flush stdout;
    let (new_row, _) = get_cursor_position () in
    { model with
      repl_output = None;
      repl_cursor = (new_row, 1);
      prompt_top_row = max model.prompt_top_row (new_row + 1);
    }

type msg =
  | Key of Tty_listener.key
  | Response of Backend.response_chunk

let handle_key backend ~term_width model key =
  match Update.update key ~term_width model with
  | Frontend_types.Exit -> `Exit
  | Frontend_types.Cancel ->
    Backend.cancel backend;
    Out_channel.flush stdout;
    `Continue (make_init ())
  | Frontend_types.Submit (text, new_model) ->
    Backend.submit backend text;
    `Continue new_model
  | Frontend_types.Continue new_model ->
    `Continue new_model

let handle_response model response =
  { model with backend_response = Some response }
  |> Update.process_response
  |> print_repl_output
  |> Update.handle_vertical_cursor_movement

let run env backend =
  let clock = Eio.Stdenv.clock env in
  let stdin = Eio.Stdenv.stdin env in
  let rec loop model =
    print_string (View.view model);
    Out_channel.flush stdout;
    let (term_width, _) = get_term_dimensions () in
    let msg =
      if model.awaiting_response then
        Eio.Fiber.any [
          (fun () -> Key (Tty_listener.await_input ~clock ~stdin));
          (fun () -> Response (Backend.await_response backend));
        ]
      else
        Key (Tty_listener.await_input ~clock ~stdin)
    in
    match msg with
    | Key key ->
      (match handle_key backend ~term_width model key with
       | `Exit -> ()
       | `Continue new_model -> loop new_model)
    | Response r ->
      loop (handle_response model r)
  in
  loop (make_init ())

let () =
  clear_log ();
  Eio_main.run @@ fun env ->
    let backend = Backend.create () in
    let orig = set_raw_mode () in
    set_solid_cursor ();
    Stdlib.Fun.protect
      (fun () -> run env backend)
      ~finally:(fun () ->
        print_endline "";
        restore_mode orig;
        Backend.deinit backend)
