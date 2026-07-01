open Frontend_types
open Text_editor

let insert_spaces count model =
  let rec loop m n = if n <= 0 then m else loop (insert_char m " ") (n - 1) in
  loop model count

let expand_braces ~inner_indent ~outer_indent model =
  model |> insert_newline |> insert_spaces inner_indent |> insert_newline
  |> insert_spaces outer_indent |> move_up |> go_to_line_end

let submit_normal_text model =
  let text =
    String.concat "\n" (List.map Unicode_string.to_string model.input.lines)
  in
  History.add_to_history model.input.history model.input.lines;
  if String.equal (String.trim text) "q()" then (model, [ Repl_effect.Quit ])
  else (Mode_common.clear_model_for_submit model, [ Repl_effect.Submit text ])

let submit model =
  match
    R_enter.action ~lines:model.input.lines ~cursor_line:model.input.cursor_line
      ~cursor_pos:model.input.cursor_pos ~cache:model.input.lex_cache
  with
  | R_enter.Submit -> submit_normal_text model
  | R_enter.Insert_newline { indent } ->
      let rec repeat_n_times n f m =
        if n <= 0 then m else repeat_n_times (n - 1) f (f m)
      in
      ( model |> insert_newline
        |> repeat_n_times indent (fun m -> insert_char m " "),
        [] )
  | R_enter.Expand_braces { inner_indent; outer_indent } ->
      (expand_braces ~inner_indent ~outer_indent model, [])

let enter_history_search model =
  {
    model with
    input =
      {
        model.input with
        mode = History_search Unicode_string.empty;
        lines = [ Unicode_string.empty ];
        lex_cache = R_lex_cache.create [ Unicode_string.empty ];
        cursor_pos = 0;
        cursor_line = 0;
        completion = None;
      };
  }

let apply_key key model =
  let open Tty_listener in
  let model =
    match key with
    | Tab | Escape | Enter -> model
    | _ -> (
        match model.input.completion with
        | Some cs when Completion.is_in_completion_mode cs ->
            { model with input = { model.input with completion = None } }
        | _ -> model)
  in
  match key with
  | Ctrl 'c' when model.repl.awaiting_response -> (model, [ Repl_effect.Cancel ])
  | Ctrl 'p' when model.repl.awaiting_response -> (model, [])
  | Ctrl 'd' ->
      if is_empty_input model then (model, [ Repl_effect.Quit ])
      else (delete_char_after_cursor model, [])
  | Enter -> (
      match Completion_controller.accept_backslash_completion model with
      | Some result -> result
      | None -> submit model)
  | Ctrl 'u' -> (delete_before_cursor model, [])
  | Ctrl '\r' -> (insert_newline model, [])
  | Ctrl 'p' -> (Mode_common.shift_history model ~amount:1, [])
  | Ctrl 'r' -> (enter_history_search model, [])
  | Ctrl 'a' -> (go_to_line_start model, [])
  | Ctrl 'e' -> (go_to_line_end model, [])
  | Other "next word" -> (go_to_next_word model, [])
  | Other "last word" -> (go_to_last_word model, [])
  | Char ";" when prompt_is_empty model ->
      ({ model with input = { model.input with mode = Shell } }, [])
  | Char ":" when prompt_is_empty model ->
      ({ model with input = { model.input with mode = Ai } }, [])
  | Char c -> (user_input_char model c, [])
  | Backspace -> (user_input_delete model, [])
  | Left -> (move_left model, [])
  | Right -> (move_right model, [])
  | Up ->
      if
        at_first_line model
        || Option.is_some model.input.flipping_through_history
      then (Mode_common.shift_history model ~amount:1, [])
      else (move_up model, [])
  | Down ->
      if
        at_last_line model
        || Option.is_some model.input.flipping_through_history
      then (Mode_common.shift_history model ~amount:(-1), [])
      else (move_down model, [])
  | Paste text -> (insert_paste model text, [])
  | Tab -> (Completion_controller.handle_tab model, [])
  | Escape -> (
      match model.input.completion with
      | Some cs when Completion.is_in_completion_mode cs ->
          let token_start = Completion.token_start cs in
          let original = Completion.original_token cs in
          let reverted = replace_token model token_start original in
          ( { reverted with input = { reverted.input with completion = None } },
            [] )
      | Some _ ->
          ({ model with input = { model.input with completion = None } }, [])
      | None -> (model, []))
  | _ -> (model, [])
