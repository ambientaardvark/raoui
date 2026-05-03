open Frontend_types
module Term = Terminal_ops.Ansi

let initial_theme user_options =
  match user_options.User_options.theme_name with
  | Some name -> Theme.of_name name
  | None -> Theme.tokyo_night

let make ~history ~user_options ~terminal_capabilities () : Frontend_types.model
    =
  let row, _col = Terminal_session.get_cursor_position () in
  let term_width, term_height = Terminal_session.get_term_dimensions () in
  let lines = [ Unicode_string.empty ] in
  let running_in_ide =
    match Sys.getenv_opt "TERM_PROGRAM" with
    | Some "vscode" -> true
    | _ -> false
  in
  Logs.info (fun m ->
      m
        "initial terminal state: row=%d width=%d height=%d ide=%b \
         image_protocol=%s"
        row term_width term_height running_in_ide
        (match terminal_capabilities.Terminal_capabilities.image_protocol with
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
    input =
      {
        lines;
        lex_cache = R_lex_cache.create lines;
        cursor_line = 0;
        cursor_pos = 0;
        previous_key = None;
        persistent_col = 0;
        history;
        flipping_through_history = None;
        completion = None;
        mode = Frontend_types.Normal;
      };
    layout =
      {
        prompt_top_row = clamped;
        term_width;
        term_height;
        prompt_box_height = Frontend_types.min_prompt_height;
        previous_prompt_top_row = row;
        scroll_amount = 0;
        running_in_ide;
      };
    repl =
      {
        awaiting_response = false;
        backend_response = None;
        repl_output = None;
        repl_cursor = (row, 1);
      };
    theme = initial_theme user_options;
  }
