open Frontend_types
open Text_editor

type msg =
  | Key of Tty_listener.key
  | Response of Ffi_backend.response_chunk
  | Backslash_effect_result of {
      token_start : int;
      original_token : string;
      inserted_text : string option;
    }
  | TermResize of int * int

let shift_history model ~amount =
  let result =
    if amount > 0 then
      History.go_back model.history ~current_prompt:model.lines ()
    else History.go_forwards model.history ~current_prompt:model.lines ()
  in
  match result with
  | Some lines ->
      {
        model with
        lines;
        lex_cache = Syntax.Cache.create lines;
        flipping_through_history = Some 2;
      }
      |> move_cursor_to_end
  | None -> model

let continuation_indent_size = 2

let tokens_before_cursor model =
  Syntax.Cache.tokens_before_line model.lex_cache ~line:(model.cursor_line + 1)

let mode_at_cursor model =
  let entry = Syntax.Cache.get_entry model.lex_cache ~line:model.cursor_line in
  match entry with
  | None -> R_lexer.Normal
  | Some entry ->
      let line = current_line model in
      let cursor_byte =
        Language_syntax.cursor_byte_offset ~line ~cursor_pos:model.cursor_pos
      in
      let prefix = String.sub entry.text 0 cursor_byte in
      let _, end_mode = R_lexer.lex_line entry.start_mode prefix in
      end_mode

let inside_empty_brackets model =
  match
    Syntax.Cache.get_line_tokens model.lex_cache ~line:model.cursor_line
  with
  | None -> false
  | Some tokens ->
      let line = current_line model in
      let cursor_byte =
        Language_syntax.cursor_byte_offset ~line ~cursor_pos:model.cursor_pos
      in
      Syntax.Continuation.inside_empty_brackets ~tokens
        ~cursor_byte_offset:cursor_byte

let leading_spaces s =
  let rec loop i =
    if i >= String.length s then i
    else if String.get s i = ' ' then loop (i + 1)
    else i
  in
  loop 0

let insert_spaces count model =
  let rec loop m n = if n <= 0 then m else loop (insert_char m " ") (n - 1) in
  loop model count

let expand_empty_brackets model =
  let line = current_line model in
  let base_indent = leading_spaces (Unicode_string.to_string line) in
  model |> insert_newline
  |> insert_spaces (base_indent + continuation_indent_size)
  |> insert_newline |> insert_spaces base_indent |> move_up |> go_to_line_end

let at_empty_line model =
  "" = (model |> current_line |> Unicode_string.to_string |> String.trim)

(* Submit handlers per mode *)

let scroll_terminal_after_submit model =
  let width = effective_width model in
  let wrapped = wrap_lines width model.lines in
  let total_rows = List.length wrapped in
  let output_row = model.prompt_top_row + total_rows in
  let new_prompt_top = output_row + 1 in
  let scroll_amount =
    if new_prompt_top > model.term_height then
      model.term_height - new_prompt_top
    else 0
  in
  (output_row, scroll_amount, new_prompt_top)

let clear_model_for_submit ?(awaiting_response = true) model =
  let output_row, scroll_amount, new_prompt_top =
    scroll_terminal_after_submit model
  in
  {
    model with
    awaiting_response;
    completion = None;
    repl_cursor = (output_row + scroll_amount, 1);
    prompt_top_row = new_prompt_top + scroll_amount;
    previous_prompt_top_row = new_prompt_top + scroll_amount;
    lines = [ Unicode_string.empty ];
    lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
    prompt_box_height = min_prompt_height;
    scroll_amount;
  }

let submit_in_shell_mode model =
  let text = Unicode_string.to_string (current_line model) in
  let rec safe_guard n =
    let g = String.make n '-' in
    let closing = ")" ^ g ^ "\"" in
    let cl = String.length closing and tl = String.length text in
    let rec found i =
      i <= tl - cl && (String.sub text i cl = closing || found (i + 1))
    in
    if tl >= cl && found 0 then safe_guard (n + 1) else g
  in
  let guard = safe_guard 1 in
  let r_command = Printf.sprintf "system(r\"%s(%s)%s\")" guard text guard in
  History.add_to_history model.history model.lines;
  ( clear_model_for_submit { model with mode = Normal },
    [ Repl_effect.Submit r_command ] )

let submit_in_readline_mode model =
  let text = Unicode_string.to_string (current_line model) in
  let new_model =
    {
      model with
      mode = Frontend_types.Normal;
      completion = None;
      lines = [ Unicode_string.empty ];
      lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
      cursor_row = 0;
      cursor_col = 0;
      cursor_line = 0;
      cursor_pos = 0;
    }
  in
  (new_model, [ Repl_effect.SubmitReadlineInput text ])

let submit_normal_text model =
  let text =
    String.concat "\n" (List.map Unicode_string.to_string model.lines)
  in
  History.add_to_history model.history model.lines;
  if String.equal (String.trim text) "q()" then (model, [ Repl_effect.Quit ])
  else (clear_model_for_submit model, [ Repl_effect.Submit text ])

let submit_in_normal_mode model =
  if inside_empty_brackets model then (expand_empty_brackets model, [])
  else if at_empty_line model then submit_normal_text model
  else
    let tokens = tokens_before_cursor model in
    match Syntax.Continuation.analyze tokens with
    | Syntax.Continuation.Submit -> submit_normal_text model
    | Syntax.Continuation.Continue { indent_levels; in_empty_brackets = _ } ->
        let line = current_line model in
        let base_indent = leading_spaces (Unicode_string.to_string line) in
        let indent_spaces =
          max base_indent (indent_levels * continuation_indent_size)
        in
        let rec repeat_n_times n f m =
          if n <= 0 then m else repeat_n_times (n - 1) f (f m)
        in
        ( model |> insert_newline
          |> repeat_n_times indent_spaces (fun m -> insert_char m " "),
          [] )

(* Submit router *)
let submit model =
  match model.mode with
  | Readline _ -> submit_in_readline_mode model
  | Shell -> submit_in_shell_mode model
  | Normal -> submit_in_normal_mode model
  | History_search _ -> ({ model with mode = Normal }, [])

let handle_vertical_cursor_movement model =
  let width = effective_width model in
  (* Completions are rendered as an overlay in view.ml and should not
     contribute to prompt box height or scrolling math. *)
  let dropdown_rows = 0 in
  let new_height =
    model.lines |> wrap_lines width |> List.length |> ( + ) dropdown_rows
    |> max model.prompt_box_height
    |> max min_prompt_height
  in
  let scrolls_from_expansion =
    if new_height > model.prompt_box_height then
      let bottom = model.prompt_top_row + new_height - 1 in
      if bottom > model.term_height then model.term_height - bottom else 0
    else 0
  in
  let prompt_top_after_expansion =
    model.prompt_top_row + scrolls_from_expansion
  in
  let cursor_term_row = model.cursor_row + prompt_top_after_expansion in
  let scrolls_from_cursor_movement =
    if cursor_term_row > model.term_height then
      model.term_height - cursor_term_row
    else if cursor_term_row < 1 then 1 - cursor_term_row
    else 0
  in
  {
    model with
    prompt_box_height = new_height;
    prompt_top_row =
      model.prompt_top_row + scrolls_from_expansion
      + scrolls_from_cursor_movement;
    scroll_amount = scrolls_from_expansion;
  }

let handle_resize new_width new_height model =
  if model.term_width = new_width && model.term_height = new_height then model
  else
    let model =
      { model with term_width = new_width; term_height = new_height }
    in
    let new_eff_width = effective_width model in
    let new_row, new_col =
      internal_to_terminal new_eff_width model.lines
        (model.cursor_line, model.cursor_pos)
    in
    let prompt_top = clamp_prompt_top new_height model.prompt_top_row in
    {
      model with
      cursor_row = new_row;
      cursor_col = new_col;
      prompt_top_row = prompt_top;
      prompt_box_height = min_prompt_height;
    }

let grapheme_cluster_count text =
  match Unicode_string.of_string text with
  | Ok u -> Ok (Unicode_string.length u)
  | Error e -> Error e

let filter_completions model =
  match model.mode with
  | History_search _ -> { model with completion = None }
  | _ -> (
      match model.completion with
      | None -> model
      | Some cs when Completion.is_in_completion_mode cs ->
          model (* completion mode: set is fixed *)
      | Some cs -> (
          let line = current_line model in
          let line_len = Unicode_string.length line in
          let token_start = Completion.token_start cs in
          if token_start > line_len || token_start > model.cursor_pos then
            { model with completion = None }
          else
            let prefix_len = model.cursor_pos - token_start in
            let prefix =
              Unicode_string.to_string
                (Unicode_string.sub line ~start:token_start ~len:prefix_len)
            in
            match Completion.filter cs ~prefix with
            | None -> { model with completion = None }
            | Some filtered -> { model with completion = Some filtered }))

let handle_tab model =
  match model.completion with
  | None -> model
  | Some cs -> (
      if List.length (Completion.filtered_items cs) = 0 then model
      else
        let cs_with_original =
          if not (Completion.is_in_completion_mode cs) then
            let line = current_line model
            and token_start = Completion.token_start cs in
            let prefix_len = model.cursor_pos - token_start in
            let original =
              Unicode_string.to_string
                (Unicode_string.sub line ~start:token_start ~len:prefix_len)
            in
            Completion.save_original_token cs ~token:original
          else cs
        in
        let cs_cycled = Completion.cycle_next cs_with_original in
        match Completion.current_completion cs_cycled with
        | None -> model
        | Some completion_item ->
            let token_start = Completion.token_start cs_cycled in
            let inserted =
              replace_token model token_start (Completion.label completion_item)
            in
            { inserted with completion = Some cs_cycled })

let accept_backslash_completion model =
  match model.completion with
  | None -> None
  | Some cs -> (
      let exact_match_item () =
        let line = current_line model in
        let token_start = Completion.token_start cs in
        if token_start > model.cursor_pos then None
        else
          let typed_len = model.cursor_pos - token_start in
          let typed_text =
            Unicode_string.sub line ~start:token_start ~len:typed_len
            |> Unicode_string.to_string
          in
          Completion.filtered_items cs
          |> List.find_opt (fun item ->
              String.equal (Completion.label item) typed_text)
      in
      let chosen_item =
        match Completion.current_completion cs with
        | Some item -> Some item
        | None -> exact_match_item ()
      in
      match chosen_item with
      | Some completion_item -> (
          match Completion.kind completion_item with
          | Completion.Backslash command -> (
              let token_start = Completion.token_start cs in
              let selected_label = Completion.label completion_item in
              match command with
              | Backslash_command.Simple { inserted_text; _ } ->
                  let replaced =
                    replace_range model token_start model.cursor_pos
                      inserted_text
                  in
                  Some ({ replaced with completion = None }, [])
              | Backslash_command.Effectful
                  { action = Backslash_command.Pick_file; _ } ->
                  Some
                    ( { model with completion = None },
                      [
                        Repl_effect.Run_backslash_effect
                          (Repl_effect.Pick_file
                             { token_start; original_token = selected_label });
                      ] ))
          | Completion.Backend -> None)
      | None -> None)

let make_backslash_completion model (token : Backslash_command.token) =
  let items =
    Backslash_command.matching_commands Backslash_command.default_registry
      ~prefix:token.command_name_prefix
    |> List.map Completion.backslash_item
  in
  if items = [] then { model with completion = None }
  else
    let completion = Completion.create ~token_start:token.token_start items in
    filter_completions { model with completion = Some completion }

let completion_followup model =
  match model.mode with
  | History_search _ -> ({ model with completion = None }, [])
  | _ ->
      let normal_completion_token_length line =
        let rec loop idx len =
          if idx < 0 then len
          else
            let cluster = Unicode_string.cluster_at line idx in
            if is_word_char cluster then loop (idx - 1) (len + 1) else len
        in
        loop (model.cursor_pos - 1) 0
      in
      let in_completion_mode =
        match model.completion with
        | Some cs when Completion.is_in_completion_mode cs -> true
        | _ -> false
      in
      if (not model.awaiting_response) && not in_completion_mode then
        match List.nth_opt model.lines model.cursor_line with
        | Some line -> (
            match
              Backslash_command.token_in_line line ~cursor_pos:model.cursor_pos
            with
            | Some token -> (make_backslash_completion model token, [])
            | None when normal_completion_token_length line < 2 ->
                ({ model with completion = None }, [])
            | None ->
                let text = Unicode_string.to_string line in
                [ Repl_effect.RequestCompletions (text, model.cursor_pos) ]
                |> fun effects -> (model, effects))
        | None -> (model, [])
      else (model, [])

(* Key handlers per mode *)

let set_mode_normal_blank model =
  {
    model with
    mode = Frontend_types.Normal;
    lines = [ Unicode_string.empty ];
    lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
  }

let apply_key_in_shell_mode key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' -> (set_mode_normal_blank model, [])
  | Ctrl 'p' | Up | Down -> (model, [])
  | Enter -> submit model
  | _ -> (
      let model =
        match key with
        | Tab | Escape -> model
        | _ -> (
            match model.completion with
            | Some cs when Completion.is_in_completion_mode cs ->
                { model with completion = None }
            | _ -> model)
      in
      match key with
      | Ctrl 'd' ->
          if is_empty_input model then (set_mode_normal_blank model, [])
          else (delete_char_after_cursor model, [])
      | Ctrl 'u' -> (delete_before_cursor model, [])
      | Ctrl 'a' -> (go_to_line_start model, [])
      | Ctrl 'e' -> (go_to_line_end model, [])
      | Other "next word" -> (go_to_next_word model, [])
      | Other "last word" -> (go_to_last_word model, [])
      | Char c -> (user_input_char model c, [])
      | Backspace when prompt_is_empty model -> (set_mode_normal_blank model, [])
      | Backspace -> (user_input_delete model, [])
      | Left -> (move_left model, [])
      | Right -> (move_right model, [])
      | Paste text -> (insert_paste model text, [])
      | _ -> (model, []))

let apply_key_in_readline_mode key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' ->
      (set_mode_normal_blank model, [ Repl_effect.SubmitReadlineInput "" ])
  | Ctrl 'p' | Up | Down -> (model, [])
  | Enter -> submit model
  | _ -> (
      let model =
        match key with
        | Tab | Escape -> model
        | _ -> (
            match model.completion with
            | Some cs when Completion.is_in_completion_mode cs ->
                { model with completion = None }
            | _ -> model)
      in
      match key with
      | Ctrl 'd' ->
          if is_empty_input model then (model, [ Repl_effect.Quit ])
          else (delete_char_after_cursor model, [])
      | Ctrl 'u' -> (delete_before_cursor model, [])
      | Ctrl 'a' -> (go_to_line_start model, [])
      | Ctrl 'e' -> (go_to_line_end model, [])
      | Other "next word" -> (go_to_next_word model, [])
      | Other "last word" -> (go_to_last_word model, [])
      | Char c -> (user_input_char model c, [])
      | Backspace -> (user_input_delete model, [])
      | Left -> (move_left model, [])
      | Right -> (move_right model, [])
      | Paste text -> (insert_paste model text, [])
      | _ -> (model, []))

let apply_key_in_normal_mode key model =
  let open Tty_listener in
  let model =
    match key with
    | Tab | Escape | Enter -> model
    | _ -> (
        match model.completion with
        | Some cs when Completion.is_in_completion_mode cs ->
            { model with completion = None }
        | _ -> model)
  in
  match key with
  | Ctrl 'c' when model.awaiting_response -> (model, [ Repl_effect.Cancel ])
  | Ctrl 'p' when model.awaiting_response -> (model, [])
  | Ctrl 'd' ->
      if is_empty_input model then (model, [ Repl_effect.Quit ])
      else (delete_char_after_cursor model, [])
  | Enter -> (
      match accept_backslash_completion model with
      | Some result -> result
      | None -> submit model)
  | Ctrl 'u' -> (delete_before_cursor model, [])
  | Ctrl '\r' -> (insert_newline model, [])
  | Ctrl 'p' -> (shift_history model ~amount:1, [])
  | Ctrl 'r' ->
      ( {
          model with
          mode = History_search Unicode_string.empty;
          lines = [ Unicode_string.empty ];
          lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
          cursor_pos = 0;
          cursor_col = 0;
          cursor_row = 0;
          cursor_line = 0;
          completion = None;
        },
        [] )
  | Ctrl 'a' -> (go_to_line_start model, [])
  | Ctrl 'e' -> (go_to_line_end model, [])
  | Other "next word" -> (go_to_next_word model, [])
  | Other "last word" -> (go_to_last_word model, [])
  | Char ";" when prompt_is_empty model ->
      ({ model with mode = Shell }, [])
  | Char c -> (user_input_char model c, [])
  | Backspace -> (user_input_delete model, [])
  | Left -> (move_left model, [])
  | Right -> (move_right model, [])
  | Up ->
      if at_first_line model || Option.is_some model.flipping_through_history
      then (shift_history model ~amount:1, [])
      else (move_up model, [])
  | Down ->
      if at_last_line model || Option.is_some model.flipping_through_history
      then (shift_history model ~amount:(-1), [])
      else (move_down model, [])
  | Paste text -> (insert_paste model text, [])
  | Tab -> (handle_tab model, [])
  | Escape -> (
      match model.completion with
      | Some cs when Completion.is_in_completion_mode cs ->
          let token_start = Completion.token_start cs in
          let original = Completion.original_token cs in
          let reverted = replace_token model token_start original in
          ({ reverted with completion = None }, [])
      | Some _ -> ({ model with completion = None }, [])
      | None -> (model, []))
  | _ -> (model, [])

(* Key handler router *)
let apply_key key model =
  match model.mode with
  | Readline _ -> apply_key_in_readline_mode key model
  | Shell -> apply_key_in_shell_mode key model
  | Normal -> apply_key_in_normal_mode key model
  | History_search _ -> History_search.apply_key key model

let universal_corrections key model =
  model |> filter_completions |> handle_vertical_cursor_movement |> fun s ->
  let flipping_through_history =
    match model.flipping_through_history with
    | Some 1 | None -> None
    | Some n -> Some (n - 1)
  in
  { s with previous_key = Some key; flipping_through_history }

let sync_internal_coords model =
  let width = effective_width model in
  let cursor_line, cursor_pos =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  { model with cursor_line; cursor_pos }

let handle_key_input key model =
  let sync model =
    match model.mode with
    | History_search _ -> model
    | _ -> sync_internal_coords model
  in
  let new_model, effects =
    model
    |> handle_resize model.term_width model.term_height
    |> sync
    |> apply_key key
  in
  (universal_corrections key new_model, effects)


let process_response model =
  match model.backend_response with
  | None -> failwith "process_response called with no backend_response"
  | Some response ->
      let repl_output =
        match response with
        | Ffi_backend.Stdout s -> Output_text [ (`Raw, s) ]
        | Ffi_backend.Result s -> Output_text [ (`Raw, s) ]
        | Ffi_backend.R_error s -> Output_text [ (`Error, s) ]
        | Ffi_backend.Internal_error s ->
            Output_text [ (`Error, "Internal error: " ^ s) ]
        | Ffi_backend.Restarted s -> Output_text [ (`Error, s) ]
        | Ffi_backend.Image image -> Output_image image
        | Ffi_backend.Done -> Output_text []
        | Ffi_backend.Shutdown -> Output_text []
        | Ffi_backend.Passthrough | Ffi_backend.Passthrough_end
        | Ffi_backend.Completions _ | Ffi_backend.Readline _ ->
            Output_text []
      in
      let awaiting_response =
        match response with
        (* Keep waiting for more output until we get a terminal response *)
        | Ffi_backend.Stdout _ | Ffi_backend.Result _ | Ffi_backend.R_error _
        | Ffi_backend.Readline _ | Ffi_backend.Image _ ->
            model.awaiting_response
        (* Terminal responses *)
        | Ffi_backend.Done | Ffi_backend.Shutdown | Ffi_backend.Internal_error _
        | Ffi_backend.Restarted _ | Ffi_backend.Passthrough
        | Ffi_backend.Passthrough_end | Ffi_backend.Completions _ ->
            false
      in
      let mode =
        match response with
        | Ffi_backend.Readline prompt ->
            let normalized_prompt = if prompt = "" then "input" else prompt in
            Frontend_types.Readline normalized_prompt
        | Ffi_backend.Done -> Frontend_types.Normal (* Reset on completion *)
        | _ -> model.mode (* Preserve current mode *)
      in
      {
        model with
        backend_response = None;
        repl_output = Some repl_output;
        awaiting_response;
        scroll_amount = 0;
        mode;
      }

let update msg model =
  let model = { model with scroll_amount = 0 } in
  match msg with
  | Key key -> (
      let m, effects = handle_key_input key model in
      match effects with [] -> completion_followup m | _ -> (m, effects))
  | Response response -> (
      match response with
      | Ffi_backend.Shutdown -> (model, [ Repl_effect.Quit ])
      | Ffi_backend.Passthrough -> (model, [ Repl_effect.EnterPassthrough ])
      | Ffi_backend.Passthrough_end -> (model, [])
      | Ffi_backend.Completions (token, items) -> (
          match model.mode with
          | History_search _ -> ({ model with completion = None }, [])
          | _ ->
              let in_completion_mode =
                match model.completion with
                | Some cs when Completion.is_in_completion_mode cs -> true
                | _ -> false
              in
              if in_completion_mode then (model, [])
              else
                let token_start = model.cursor_pos - String.length token in
                let completion =
                  Completion.create ~token_start
                    (List.map Completion.backend_item items)
                in
                let m = { model with completion = Some completion } in
                (filter_completions m, []))
      | Ffi_backend.Restarted _ ->
          let m =
            { model with backend_response = Some response }
            |> process_response |> handle_vertical_cursor_movement
          in
          ( m,
            [
              Repl_effect.BackgroundSubmit
                (Printf.sprintf "options(width=%d)" model.term_width);
            ] )
      | _ ->
          let m =
            { model with backend_response = Some response }
            |> process_response |> handle_vertical_cursor_movement
          in
          (m, []))
  | Backslash_effect_result { token_start; original_token; inserted_text } ->
      let original_len =
        grapheme_cluster_count original_token |> Result.get_ok
      in
      let token_end = token_start + original_len in
      let m =
        match inserted_text with
        | None -> { model with completion = None }
        | Some text ->
            let replaced = replace_range model token_start token_end text in
            { replaced with completion = None }
      in
      (m, [])
  | TermResize (width, height) ->
      let m =
        handle_resize width height model |> handle_vertical_cursor_movement
      in
      ( m,
        [
          Repl_effect.BackgroundSubmit
            (Printf.sprintf "options(width=%d)" width);
        ] )
