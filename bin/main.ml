open Base
open Stdio
open Raoui
open Frontend_types

let set_raw_mode () =
  let termio = Unix.tcgetattr Unix.stdin in
  let raw_termio =
    {
      termio with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_isig = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
    }
  in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_termio;
  termio

let restore_mode termio = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

let set_solid_cursor () =
  print_string "\x1b[2 q";
  Out_channel.flush stdout

let enable_bracketed_paste () =
  print_string "\x1b[?2004h";
  Out_channel.flush stdout

let disable_bracketed_paste () =
  print_string "\x1b[?2004l";
  Out_channel.flush stdout

let clear_log () = Out_channel.write_all "/Users/alanlee/Documents/Programs/raoui/debug_log.txt" ~data:""

let get_cursor_position () =
  print_string "\x1b[6n";
  Out_channel.flush stdout;
  let buf = Buffer.create 16 in
  let rec read_until_r () =
    let c = In_channel.(input_char stdin) |> Option.value_exn in
    if Char.(c = 'R') then ()
    else (
      Buffer.add_char buf c;
      read_until_r ())
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

let rec await_dim_change prev_width prev_height =
  let w, h = get_term_dimensions () in
  if w = prev_width && h = prev_height then (
    Eio.Fiber.yield ();
    await_dim_change w h)
  else (w, h)

let history_path =
  match Stdlib.Sys.getenv_opt "HOME" with
  | Some home -> home ^ "/.raoui_history.db"
  | None -> ".raoui_history.db"

let history = lazy (History.init history_path)

let make_init () : Frontend_types.model =
  let row, _col = get_cursor_position () in
  let term_width, term_height = get_term_dimensions () in
  let lines = [ Unicode_string.empty ] in
  let running_in_ide =
    match Stdlib.Sys.getenv_opt "TERM_PROGRAM" with
    | Some "vscode" -> true
    | _ -> false
  in
  {
    lines;
    lex_cache = Update.lex_cache_for_lines lines;
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
    prompt_top_row = row;
    term_width;
    term_height;
    prompt_box_height = 1;
    previous_prompt_top_row = row;
    previous_key = None;
    persistent_col = 0;
    awaiting_response = false;
    backend_response = None;
    repl_output = None;
    repl_cursor = (row, 1);
    scroll_amount = 0;
    history = Lazy.force history;
    flipping_through_history = None;
    running_in_ide;
  }

let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col

(* NOTE: This clears from repl_cursor to end of screen before printing output,
   which erases any prompt the user typed while waiting. View then repaints.
   This may not work correctly with output that uses cursor movement (e.g.
   progress bars with \r) - get_cursor_position won't reflect actual extent. *)
let print_repl_output model =
  match model.repl_output with
  | None | Some [] -> { model with repl_output = None }
  | Some spans ->
      let row, col = model.repl_cursor in
      print_string (cursor_to row col);
      print_string "\x1b[J";
      print_string (Terminal_ops.render_spans spans);
      Out_channel.flush stdout;
      let new_row, _ = get_cursor_position () in
      {
        model with
        repl_output = None;
        repl_cursor = (new_row, 1);
        prompt_top_row = max model.prompt_top_row (new_row + 1);
      }

type msg =
  | Key of Tty_listener.key
  | Response of Ffi_backend.response_chunk
  | Term_size of (int * int)

let handle_key backend model key =
  match Update.update key model with
  | Frontend_types.Exit -> `Exit
  | Frontend_types.Cancel ->
      Ffi_backend.cancel backend;
      Out_channel.flush stdout;
      `Continue (make_init ())
  | Frontend_types.Submit (text, _) when String.equal (String.strip text) "q()"
    ->
      `Exit
  | Frontend_types.Submit (text, new_model) ->
      Ffi_backend.submit backend text;
      `Continue new_model
  | Frontend_types.Continue new_model -> `Continue new_model

let handle_response model response =
  { model with backend_response = Some response }
  |> Update.process_response |> print_repl_output
  |> Update.handle_vertical_cursor_movement

let run env backend =
  let clock = Eio.Stdenv.clock env in
  let stdin = Eio.Stdenv.stdin env in
  let cached_keys = Tty_listener.drain_to_keys ~clock ~stdin in
  let init_model = make_init () in
  let init_width = init_model.term_width in
  Ffi_backend.background_submit backend (Printf.sprintf "options(width=%d)" init_width);
  let model_after_cached =
    Stdlib.List.fold_left
      (fun m key ->
        match handle_key backend m key with
        | `Exit -> m
        | `Continue m' -> m')
      init_model
      cached_keys
  in
  let rec loop model =
    let _ = Ffi_backend.poll_ready backend in
    print_string (View.view model);
    Out_channel.flush stdout;
    let msg =
        Eio.Fiber.any
          [
            (fun () -> Key (Tty_listener.await_input ~clock ~stdin));
            (fun () -> Response (Ffi_backend.await_response backend));
            (fun () ->
              Term_size (await_dim_change model.term_width model.term_height));
          ]
    in
    match msg with
    | Key key -> (
        match handle_key backend model key with
        | `Exit -> ()
        | `Continue new_model -> loop new_model)
    | Response Ffi_backend.Shutdown -> ()
    | Response (Ffi_backend.Restarted _ as r) ->
        Ffi_backend.background_submit backend
          (Printf.sprintf "options(width=%d)" model.term_width);
        loop (handle_response model r)
    | Response r -> loop (handle_response model r)
    | Term_size (term_width, term_height) ->
        Ffi_backend.background_submit backend
          (Printf.sprintf "options(width=%d)" term_width);
        loop { model with term_width; term_height }
  in
  loop model_after_cached

let () =
  clear_log ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let backend = Ffi_backend.create ~sw () in
  let orig = set_raw_mode () in
  set_solid_cursor ();
  enable_bracketed_paste ();
  Stdlib.Fun.protect
    (fun () -> run env backend)
    ~finally:(fun () ->
      disable_bracketed_paste ();
      print_endline "";
      restore_mode orig;
      if Lazy.is_val history then History.close (Lazy.force history);
      Ffi_backend.deinit backend)
