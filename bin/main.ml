open Raoui
open Frontend_types

module Term = Terminal_ops.Ansi

module V = View.Make (Term)

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
  print_string Term.solid_cursor;
  flush stdout

let enable_bracketed_paste () =
  print_string Term.enable_bracketed_paste;
  flush stdout

let disable_bracketed_paste () =
  print_string Term.disable_bracketed_paste;
  flush stdout

let get_cursor_position () =
  print_string Term.cursor_position_request;
  flush stdout;
  let buf = Buffer.create 16 in
  let rec read_until_r () =
    let c = input_char stdin in
    if c = 'R' then ()
    else (
      Buffer.add_char buf c;
      read_until_r ())
  in
  read_until_r ();
  let response = Buffer.contents buf in
  Scanf.sscanf response "\x1b[%d;%d" (fun row col -> (row, col))

let get_term_dimensions () =
  let height =
    Terminal_size.get_rows ()
    |> function
    | Some h -> h
    | None -> failwith "can't get terminal height"
  in
  let width =
    Terminal_size.get_columns ()
    |> function
    | Some w -> w
    | None -> failwith "can't get terminal width"
  in
  (width, height)

let sigwinch_pipe_rd, sigwinch_pipe_wr =
  let rd, wr = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock rd;
  Unix.set_nonblock wr;
  rd, wr

let sigwinch_byte = Bytes.of_string "x"

let () =
  Sys.set_signal (Sys.sigwinch) (Sys.Signal_handle (fun _ ->
    try ignore (Unix.write sigwinch_pipe_wr sigwinch_byte 0 1)
    with Unix.Unix_error _ -> ()
  ))

let rec await_dim_change ~current_w ~current_h =
  Eio_unix.await_readable sigwinch_pipe_rd;
  (* Drain all pending bytes *)
  let buf = Bytes.create 16 in
  (try while Unix.read sigwinch_pipe_rd buf 0 16 > 0 do () done
   with Unix.Unix_error (Unix.EAGAIN, _, _) -> ());
  let w, h = get_term_dimensions () in
  if w = current_w && h = current_h then
    await_dim_change ~current_w ~current_h
  else (w, h)

let history_path =
  match Sys.getenv_opt "HOME" with
  | Some home -> home ^ "/.raoui_history.txt"
  | None -> ".raoui_history.txt"

let history = lazy (History.init history_path)

let make_init () : Frontend_types.model =
  let row, _col = get_cursor_position () in
  let term_width, term_height = get_term_dimensions () in
  let lines = [ Unicode_string.empty ] in
  let running_in_ide =
    match Sys.getenv_opt "TERM_PROGRAM" with
    | Some "vscode" -> true
    | _ -> false
  in
  Logs.info (fun m ->
      m "initial terminal state: row=%d width=%d height=%d ide=%b"
        row term_width term_height running_in_ide);
  let clamped = Frontend_types.clamp_prompt_top term_height row in
  let scroll_needed = row - clamped in
  if scroll_needed > 0 then begin
    print_string (Term.scroll_up ~term_height scroll_needed);
    flush stdout
  end;
  {
    lines;
    lex_cache = Syntax.Cache.create lines;
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
    prompt_top_row = clamped;
    term_width;
    term_height;
    prompt_box_height = Frontend_types.min_prompt_height;
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
    completion = None;
    mode = Frontend_types.Normal;
  }

(* NOTE: This clears from repl_cursor to end of screen before printing output,
   which erases any prompt the user typed while waiting. View then repaints.
   This may not work correctly with output that uses cursor movement (e.g.
   progress bars with \r) - get_cursor_position won't reflect actual extent. *)
let print_repl_output model =
  match model.repl_output with
  | None -> model
  | Some [] ->
      (* Done - reserve the bottom prompt zone. *)
      let new_row, new_col = get_cursor_position () in
      let next_prompt_row = if new_col = 1 then new_row else new_row + 1 in
      let natural = max model.prompt_top_row next_prompt_row in
      let clamped = Frontend_types.clamp_prompt_top model.term_height natural in
      let scroll_needed = natural - clamped in
      if scroll_needed > 0 then begin
        print_string (Term.scroll_up ~term_height:model.term_height scroll_needed);
        flush stdout
      end;
      {
        model with
        repl_output = None;
        repl_cursor = (new_row - scroll_needed, new_col);
        prompt_top_row = clamped;
        prompt_box_height = Frontend_types.min_prompt_height;
      }
  | Some spans ->
      let row, col = model.repl_cursor in
      print_string (Term.cursor_to row col);
      print_string Term.clear_to_eos;
      print_string (Term.render_spans spans);
      flush stdout;
      let new_row, new_col = get_cursor_position () in
      let next_prompt_row = if new_col = 1 then new_row else new_row + 1 in
      {
        model with
        repl_output = None;
        repl_cursor = (new_row, new_col);
        prompt_top_row = max model.prompt_top_row next_prompt_row;
      }

let execute_one backend = function
  | Repl_effect.Submit text ->
      Ffi_backend.submit backend text
  | Repl_effect.Cancel ->
      Ffi_backend.cancel backend
  | Repl_effect.RequestCompletions (text, cursor_pos) ->
      Ffi_backend.request_completions backend text ~cursor_pos
  | Repl_effect.SubmitReadlineInput text ->
      Ffi_backend.submit_readline_input text
  | Repl_effect.BackgroundSubmit text ->
      Ffi_backend.background_submit backend text
  | Repl_effect.EnterPassthrough -> ()
  | Repl_effect.Quit -> ()

let execute_effects backend effects =
  List.iter (execute_one backend) effects

let enter_passthrough model backend orig_termios loop =
  restore_mode orig_termios;
  disable_bracketed_paste ();
  Ffi_backend.signal_passthrough ();
  let rec passthrough_loop () =
    match Ffi_backend.await_response backend with
    | Ffi_backend.Passthrough_end ->
        ignore (set_raw_mode ());
        enable_bracketed_paste ();
        let new_row, new_col = get_cursor_position () in
        let next_prompt_row = if new_col = 1 then new_row else new_row + 1 in
        let natural = max model.prompt_top_row next_prompt_row in
        let clamped = Frontend_types.clamp_prompt_top model.term_height natural in
        let scroll_needed = natural - clamped in
        if scroll_needed > 0 then begin
          print_string (Term.scroll_up ~term_height:model.term_height scroll_needed);
          flush stdout
        end;
        loop { model with
          prompt_top_row = clamped;
          repl_cursor = (new_row - scroll_needed, new_col);
          prompt_box_height = Frontend_types.min_prompt_height;
        }
    | _ -> passthrough_loop ()
  in
  passthrough_loop ()

let run env backend ~orig_termios =
  let clock = Eio.Stdenv.clock env
  and stdin = Eio.Stdenv.stdin env in
  let cached_keys =
    Tty_listener.drain_to_keys_with_timeouts ~clock ~stdin
      ~escape_timeout_sec:0.2
      ~settle_timeout_sec:0.2
  in

  let init_model = make_init () in
  let init_width = init_model.term_width in
  let startup_file =
    let dir = Filename.dirname Sys.executable_name in
    let bundled = Filename.concat dir "startup.R" in
    if Sys.file_exists bundled then bundled else "r_scripts/startup.R"
  in
  Logs.info (fun m ->
      m "startup: cwd=%s exe=%s term_program=%s vscode_init_r=%s startup_file=%s"
        (Sys.getcwd ())
        Sys.executable_name
        (Option.value ~default:"" (Sys.getenv_opt "TERM_PROGRAM"))
        (Option.value ~default:"" (Sys.getenv_opt "VSCODE_INIT_R"))
        startup_file);

  Ffi_backend.background_submit backend
    (Printf.sprintf "source('%s');options(width=%d)" startup_file init_width);

  let model_after_cached =
    List.fold_left
      (fun m key ->
        let m', effects = Update.update (Update.Key key) m in
        match effects with
        | [Repl_effect.Quit] -> m
        | _ ->
          execute_effects backend effects;
          m')
      init_model
      cached_keys
  in

  let rec loop model =
    let _ = Ffi_backend.poll_ready backend in
    print_string (V.view model);
    flush stdout;
    let msg =
        Eio.Fiber.any
          [
            (fun () ->
              let escape_timeout_sec =
                if model.running_in_ide then 0.2 else 0.05
              in
              Update.Key
                (Tty_listener.await_input_with_timeout ~escape_timeout_sec
                   ~clock ~stdin));
            (fun () -> Update.Response (Ffi_backend.await_response backend));
            (fun () ->
              let w, h =
                await_dim_change ~current_w:model.term_width ~current_h:model.term_height
              in
              Update.TermResize (w, h));
          ]
    in
    let new_model, effects = Update.update msg model in
    match effects with
    | [Repl_effect.Quit] -> ()
    | [Repl_effect.Cancel] ->
        execute_effects backend effects;
        loop (make_init ())
    | [Repl_effect.EnterPassthrough] ->
        execute_effects backend effects;
        enter_passthrough new_model backend orig_termios loop
    | _ ->
        execute_effects backend effects;
        loop (print_repl_output new_model)
  in
  loop model_after_cached

let () =
  Printexc.record_backtrace true;
  let log_path = App_log.init () in
  Rffi.set_crash_log_path log_path;
  Logs.app (fun m -> m "logging to %s" log_path);
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in
    let backend = Ffi_backend.create ~sw ~clock () in
    let orig = set_raw_mode () in
    set_solid_cursor ();
    enable_bracketed_paste ();
    Fun.protect
      (fun () -> run env backend ~orig_termios:orig)
      ~finally:(fun () ->
        disable_bracketed_paste ();
        print_endline "";
        restore_mode orig;
        if Lazy.is_val history then History.close (Lazy.force history);
        Ffi_backend.deinit backend)
  with exn ->
    App_log.log_exception ~context:"fatal toplevel exception" exn;
    raise exn
