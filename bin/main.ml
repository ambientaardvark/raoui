open Raoui
open Frontend_types
module Term = Terminal_ops.Ansi
module V = View.Make (Term)

let paths = Paths.resolve ()
let history = lazy (History.init paths.history_file)
let user_options = User_options.read paths.options_file
let terminal_capabilities = Terminal_capabilities.detect ()

let make_init () =
  Initial_model.make ~history:(Lazy.force history) ~user_options
    ~terminal_capabilities ()

let print_repl_output =
  Output_renderer.print_repl_output ~terminal_capabilities ~user_options

let execute_one backend ai_backend = function
  | Repl_effect.Submit text -> Ffi_backend.submit backend text
  | Repl_effect.Cancel -> Ffi_backend.cancel backend
  | Repl_effect.RequestCompletions (text, cursor_pos) ->
      Ffi_backend.request_completions backend text ~cursor_pos
  | Repl_effect.SubmitReadlineInput text ->
      Ffi_backend.submit_readline_input text
  | Repl_effect.BackgroundSubmit text ->
      Ffi_backend.background_submit backend text
  | Repl_effect.SubmitAiQuery query -> Ai_backend.submit_query ai_backend query
  | Repl_effect.Run_backslash_effect _ -> ()
  | Repl_effect.EnterPassthrough -> ()
  | Repl_effect.Quit -> ()

let execute_effects backend ai_backend effects =
  List.iter (execute_one backend ai_backend) effects

let render_submit_snapshot model effects =
  let should_render_snapshot =
    List.exists
      (function
        | Repl_effect.Submit _ | Repl_effect.SubmitReadlineInput _
        | Repl_effect.SubmitAiQuery _ ->
            true
        | _ -> false)
      effects
  in
  if should_render_snapshot then begin
    let snapshot_top = Mode_common.submit_aligned_prompt_top model in
    print_string
      (V.view
         {
           model with
           layout = { model.layout with prompt_top_row = snapshot_top };
         });
    flush stdout
  end

let enter_passthrough model backend orig_termios loop =
  Terminal_session.restore_mode orig_termios;
  Terminal_session.disable_bracketed_paste ();
  Ffi_backend.signal_passthrough ();
  let rec passthrough_loop () =
    match Ffi_backend.await_response backend with
    | Ffi_backend.Passthrough_end ->
        ignore (Terminal_session.set_raw_mode ());
        Terminal_session.enable_bracketed_paste ();
        let new_row, new_col = Terminal_session.get_cursor_position () in
        let next_prompt_row = if new_col = 1 then new_row else new_row + 1 in
        let natural = max model.layout.prompt_top_row next_prompt_row in
        let clamped =
          Frontend_types.clamp_prompt_top model.layout.term_height natural
        in
        let scroll_needed = natural - clamped in
        if scroll_needed > 0 then begin
          print_string
            (Term.scroll_up ~term_height:model.layout.term_height scroll_needed);
          flush stdout
        end;
        loop
          {
            model with
            layout =
              {
                model.layout with
                prompt_top_row = clamped;
                prompt_box_height = Frontend_types.min_prompt_height;
              };
            repl =
              {
                model.repl with
                repl_cursor = (new_row - scroll_needed, new_col);
              };
          }
    | _ -> passthrough_loop ()
  in
  passthrough_loop ()

let run env backend ai_backend ~orig_termios =
  let clock = Eio.Stdenv.clock env and stdin = Eio.Stdenv.stdin env in
  let init_model = make_init () in
  let init_width = init_model.layout.term_width in
  let startup_mode =
    Plot_policy.resolve ~running_in_ide:init_model.layout.running_in_ide
      ~terminal_caps:terminal_capabilities ~plot_mode:user_options.plot_mode
  in
  Logs.info (fun m ->
      m "plot startup mode: %s"
        (Plot_policy.string_of_startup_mode startup_mode));
  let startup_file =
    let dir = Filename.dirname Sys.executable_name in
    let bundled = Filename.concat dir "startup.R" in
    if Sys.file_exists bundled then bundled else "r_scripts/startup.R"
  in
  Logs.info (fun m ->
      m
        "startup: cwd=%s exe=%s term_program=%s vscode_init_r=%s \
         startup_file=%s"
        (Sys.getcwd ()) Sys.executable_name
        (Option.value ~default:"" (Sys.getenv_opt "TERM_PROGRAM"))
        (Option.value ~default:"" (Sys.getenv_opt "VSCODE_INIT_R"))
        startup_file);

  Ffi_backend.background_submit backend
    (Printf.sprintf "source('%s');options(width=%d)" startup_file init_width);
  (match
     Plot_policy.startup_command startup_mode
       ~renderer:user_options.plot_renderer
   with
  | Some command ->
      Logs.info (fun m -> m "plot startup command: %s" command);
      Ffi_backend.background_submit backend command
  | None -> ());

  let cached_keys =
    Tty_listener.drain_to_keys_with_timeouts
      ~prefetched:(Some Terminal_session.pending_input) ~clock ~stdin
      ~escape_timeout_sec:0.2 ~settle_timeout_sec:0.2
  in

  let model_after_cached =
    List.fold_left
      (fun m key ->
        let m', effects = Update.update (Update.Key key) m in
        match effects with
        | [ Repl_effect.Quit ] -> m
        | _ ->
            execute_effects backend ai_backend effects;
            m')
      init_model cached_keys
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
              if model.layout.running_in_ide then 0.2 else 0.05
            in
            Update.Key
              (Tty_listener.await_input_with_timeout
                 ~prefetched:(Some Terminal_session.pending_input)
                 ~escape_timeout_sec ~clock ~stdin));
          (fun () -> Update.Response (Ffi_backend.await_response backend));
          (fun () -> Update.Response (Ai_backend.await_response ai_backend));
          (fun () ->
            let w, h =
              Terminal_session.await_dim_change
                ~current_w:model.layout.term_width
                ~current_h:model.layout.term_height
            in
            Update.TermResize (w, h));
        ]
    in
    let new_model, effects = Update.update msg model in
    match effects with
    | [ Repl_effect.Quit ] -> ()
    | [ Repl_effect.Cancel ] ->
        execute_effects backend ai_backend effects;
        History.record_cancel model.input.history;
        loop (make_init ())
    | [ Repl_effect.Run_backslash_effect cmd ] ->
        let result_msg = Backslash_effect_runner.run ~orig_termios cmd in
        let effect_model, effect_effects = Update.update result_msg new_model in
        execute_effects backend ai_backend effect_effects;
        loop (print_repl_output effect_model)
    | [ Repl_effect.EnterPassthrough ] ->
        execute_effects backend ai_backend effects;
        enter_passthrough new_model backend orig_termios loop
    | _ ->
        render_submit_snapshot model effects;
        execute_effects backend ai_backend effects;
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
    (* In-process MCP server exposing session history to the AI subprocess. *)
    let mcp_port =
      Mcp_server.start ~sw ~net:(Eio.Stdenv.net env)
        ~history:(Lazy.force history)
    in
    let ai_backend =
      Ai_backend.create ~sw ~process_mgr:(Eio.Stdenv.process_mgr env) ~mcp_port
        ()
    in
    let orig = Terminal_session.set_raw_mode () in
    Terminal_session.set_solid_cursor ();
    Terminal_session.enable_bracketed_paste ();
    Fun.protect
      (fun () -> run env backend ai_backend ~orig_termios:orig)
      ~finally:(fun () ->
        Terminal_session.disable_bracketed_paste ();
        print_endline "";
        Terminal_session.restore_mode orig;
        if Lazy.is_val history then History.close (Lazy.force history);
        Paths.cleanup_stale_plot_sessions paths;
        Ffi_backend.deinit backend)
  with exn ->
    App_log.log_exception ~context:"fatal toplevel exception" exn;
    raise exn
