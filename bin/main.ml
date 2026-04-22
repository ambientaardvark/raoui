open Raoui
open Frontend_types

module Term = Terminal_ops.Ansi

module V = View.Make (Term)

let paths = Paths.resolve ()

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

(* Populated during init (before Eio) by get_cursor_position, then drained
   by the Eio event loop via tty_listener's prefetched-byte mechanism. *)
let pending_input = Queue.create ()

let enqueue_pending_char c = Queue.push c pending_input

let enqueue_pending_string s = String.iter enqueue_pending_char s

let is_csi_final_byte c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '~'

let parse_cursor_position_response response =
  try Some (Scanf.sscanf response "\x1b[%d;%dR" (fun row col -> (row, col)))
  with Scanf.Scan_failure _ | End_of_file | Failure _ -> None

let get_cursor_position () =
  print_string Term.cursor_position_request;
  flush stdout;
  let rec read_csi_sequence buf =
    let c = input_char stdin in
    Buffer.add_char buf c;
    if is_csi_final_byte c then Buffer.contents buf else read_csi_sequence buf
  in
  let rec loop () =
    match input_char stdin with
    | '\x1b' -> (
        match input_char stdin with
        | '[' ->
            let buf = Buffer.create 16 in
            Buffer.add_string buf "\x1b[";
            let response = read_csi_sequence buf in
            (match parse_cursor_position_response response with
             | Some pos -> pos
             | None ->
                 enqueue_pending_string response;
                 loop ())
        | c ->
            enqueue_pending_char '\x1b';
            enqueue_pending_char c;
            loop ())
    | c ->
        enqueue_pending_char c;
        loop ()
  in
  loop ()

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

let history = lazy (History.init paths.history_file)
let user_options = User_options.read paths.options_file
let terminal_capabilities = Terminal_capabilities.detect ()

let initial_theme () =
  match user_options.theme_name with
  | Some name -> Theme.of_name name
  | None -> Theme.tokyo_night

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
      m "initial terminal state: row=%d width=%d height=%d ide=%b image_protocol=%s"
        row term_width term_height running_in_ide
        (match terminal_capabilities.image_protocol with
         | Terminal_capabilities.Kitty -> "kitty"
         | Terminal_capabilities.ITerm -> "iterm"
         | Terminal_capabilities.No_image -> "none"));
  let clamped = Frontend_types.clamp_prompt_top term_height row in
  let scroll_needed = row - clamped in
  if scroll_needed > 0 then begin
      print_string (Term.scroll_up ~term_height scroll_needed);
      flush stdout
  end;
  {
    lines;
    lex_cache = R_lex_cache.create lines;
    theme = initial_theme ();
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

let supports_file_hyperlinks =
  match terminal_capabilities.image_protocol with
  | Terminal_capabilities.Kitty | Terminal_capabilities.ITerm -> true
  | Terminal_capabilities.No_image -> false

let percent_encode_path path =
  let is_unreserved = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'
    | '-' | '_' | '.' | '~' | '/' -> true
    | _ -> false
  in
  let buf = Buffer.create (String.length path + 16) in
  String.iter
    (fun c ->
      if is_unreserved c then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    path;
  Buffer.contents buf

let file_url path = "file://" ^ percent_encode_path path

let plot_banner_spans path =
  if supports_file_hyperlinks then
    [
      ( `Raw,
        Printf.sprintf
          "\x1b]8;;%s\x1b\\[click to open plot]\x1b]8;;\x1b\\\n"
          (file_url path) );
    ]
  else
    [ (`Accent, "[open plot] "); (`Plain, path); (`Comment, "\n") ]

let image_output_spans (image : Ffi_backend.image) =
  let basename = Filename.basename image.source_path in
  let dims =
    match (image.width_px, image.height_px) with
    | Some w, Some h -> Printf.sprintf " (%dx%d)" w h
    | _ -> ""
  in
  let mime =
    match image.mime_type with
    | Some mime -> Printf.sprintf " %s" mime
    | None -> ""
  in
  [
    (`Accent, "[image] ");
    (`Plain, basename);
    (`Comment, dims ^ mime ^ "\n");
  ]
  @ plot_banner_spans image.source_path

(* NOTE: This clears from repl_cursor to end of screen before printing output,
   which erases any prompt the user typed while waiting. View then repaints.
   This may not work correctly with output that uses cursor movement (e.g.
   progress bars with \r) - get_cursor_position won't reflect actual extent. *)
let print_repl_output model =
  match model.repl_output with
  | None -> model
  | Some (Output_text []) ->
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
  | Some (Output_text _ | Output_image _ as output) ->
      let row, col = model.repl_cursor in
      let rendered_image =
        match output with
        | Output_image image ->
            Terminal_image.render ~terminal_capabilities ~config:user_options
              ~term_width:model.term_width ~image
        | Output_text _ -> None
      in
      let spans =
        match output with
        | Output_text spans -> spans
        | Output_image image -> (
            match rendered_image with
            | Some rendered ->
                let prefix = if col = 1 then "\n" else "\n\n" in
                (* Raw adds SGR resets around the content, which is harmless for
                   kitty APC sequences but would clobber SGR from other renderers. *)
                [ (`Raw, prefix ^ rendered.Terminal_image.output) ]
                @ plot_banner_spans image.source_path
            | None ->
                image_output_spans image)
      in
      print_string (Term.cursor_to row col);
      print_string Term.clear_to_eos;
      print_string (Term.render_spans model.theme spans);
      flush stdout;
      (match output, rendered_image with
       | Output_image image, Some _
         when image.preview_path <> image.source_path ->
           (try Sys.remove image.preview_path with Sys_error _ -> ())
       | _ -> ());
      let new_row, new_col = get_cursor_position () in
      let next_prompt_row = if new_col = 1 then new_row else new_row + 1 in
      {
        model with
        repl_output = None;
        repl_cursor = (new_row, new_col);
        prompt_top_row = max model.prompt_top_row next_prompt_row;
      }

let quote_r_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let run_command_capture command =
  let ic = Unix.open_process_in command in
  let status = ref (Unix.WEXITED 1) in
  let output =
    Fun.protect
      (fun () -> In_channel.input_all ic)
      ~finally:(fun () -> status := Unix.close_process_in ic)
  in
  match !status with
  | Unix.WEXITED 0 ->
      let result = String.trim output in
      if result = "" then None else Some result
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> None

let with_normal_terminal ~orig_termios f =
  restore_mode orig_termios;
  disable_bracketed_paste ();
  Fun.protect f ~finally:(fun () ->
      ignore (set_raw_mode ());
      enable_bracketed_paste ())

let choose_file ~orig_termios =
  let command = "osascript -e 'POSIX path of (choose file)'" in
  with_normal_terminal ~orig_termios (fun () -> run_command_capture command)

let run_backslash_effect ~orig_termios = function
  | Repl_effect.Pick_file { token_start; original_token } ->
      let inserted_text =
        choose_file ~orig_termios |> Option.map quote_r_string
      in
      Update.Backslash_effect_result
        {
          token_start;
          original_token;
          inserted_text;
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
  | Repl_effect.Run_backslash_effect _ -> ()
  | Repl_effect.EnterPassthrough -> ()
  | Repl_effect.Quit -> ()

let execute_effects backend effects =
  List.iter (execute_one backend) effects

let render_submit_snapshot model effects =
  let should_render_snapshot =
    List.exists
      (function
        | Repl_effect.Submit _ | Repl_effect.SubmitReadlineInput _ -> true
        | _ -> false)
      effects
  in
  if should_render_snapshot then begin
    let snapshot_top = Mode_common.submit_aligned_prompt_top model in
    print_string (V.view { model with prompt_top_row = snapshot_top });
    flush stdout
  end

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
  let init_model = make_init () in
  let init_width = init_model.term_width in
  let startup_mode =
    Plot_policy.resolve ~running_in_ide:init_model.running_in_ide
      ~terminal_caps:terminal_capabilities
      ~plot_mode:user_options.plot_mode
  in
  Logs.info (fun m ->
      m "plot startup mode: %s" (Plot_policy.string_of_startup_mode startup_mode));
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
  (match Plot_policy.startup_command startup_mode
           ~renderer:user_options.plot_renderer with
   | Some command ->
       Logs.info (fun m -> m "plot startup command: %s" command);
       Ffi_backend.background_submit backend command
   | None -> ());

  let cached_keys =
    Tty_listener.drain_to_keys_with_timeouts ~prefetched:(Some pending_input) ~clock
      ~stdin ~escape_timeout_sec:0.2 ~settle_timeout_sec:0.2
  in

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
                (Tty_listener.await_input_with_timeout ~prefetched:(Some pending_input)
                   ~escape_timeout_sec ~clock ~stdin));
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
    | [Repl_effect.Run_backslash_effect cmd] ->
        let result_msg = run_backslash_effect ~orig_termios cmd in
        let effect_model, effect_effects = Update.update result_msg new_model in
        execute_effects backend effect_effects;
        loop (print_repl_output effect_model)
    | [Repl_effect.EnterPassthrough] ->
        execute_effects backend effects;
        enter_passthrough new_model backend orig_termios loop
    | _ ->
        render_submit_snapshot model effects;
        execute_effects backend effects;
        loop (print_repl_output new_model)
  in
  loop model_after_cached

let () =
  Printexc.record_backtrace true;
  Paths.ensure_runtime_dirs paths;
  Paths.export_env paths;
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
        Paths.cleanup_stale_plot_sessions paths;
        Ffi_backend.deinit backend)
  with exn ->
    App_log.log_exception ~context:"fatal toplevel exception" exn;
    raise exn
