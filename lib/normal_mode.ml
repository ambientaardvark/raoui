open Frontend_types
open Text_editor

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
        lex_cache = R_syntax.Cache.create lines;
        flipping_through_history = Some 2;
      }
      |> move_cursor_to_end
  | None -> model

let continuation_indent_size = 2

let tokens_before_cursor model =
  R_syntax.Cache.tokens_before_line model.lex_cache ~line:(model.cursor_line + 1)

let inside_empty_brackets model =
  match
    R_syntax.Cache.get_line_tokens model.lex_cache ~line:model.cursor_line
  with
  | None -> false
  | Some tokens ->
      let line = current_line model in
      let cursor_byte =
        Lexer_cache.cursor_byte_offset ~line ~cursor_pos:model.cursor_pos
      in
      R_syntax.Continuation.inside_empty_brackets ~tokens
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

let submit_normal_text model =
  let text =
    String.concat "\n" (List.map Unicode_string.to_string model.lines)
  in
  History.add_to_history model.history model.lines;
  if String.equal (String.trim text) "q()" then (model, [ Repl_effect.Quit ])
  else
    ( Mode_common.clear_model_for_submit model,
      [ Repl_effect.Submit text ] )

let submit model =
  if inside_empty_brackets model then (expand_empty_brackets model, [])
  else if at_empty_line model then submit_normal_text model
  else
    let tokens = tokens_before_cursor model in
    match R_syntax.Continuation.analyze tokens with
    | R_syntax.Continuation.Submit -> submit_normal_text model
    | R_syntax.Continuation.Continue { indent_levels; in_empty_brackets = _ } ->
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

let enter_history_search model =
  {
    model with
    mode = History_search Unicode_string.empty;
    lines = [ Unicode_string.empty ];
    lex_cache = R_syntax.Cache.create [ Unicode_string.empty ];
    cursor_pos = 0;
    cursor_col = 0;
    cursor_row = 0;
    cursor_line = 0;
    completion = None;
  }

let apply_key key model =
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
      match Completion_controller.accept_backslash_completion model with
      | Some result -> result
      | None -> submit model)
  | Ctrl 'u' -> (delete_before_cursor model, [])
  | Ctrl '\r' -> (insert_newline model, [])
  | Ctrl 'p' -> (shift_history model ~amount:1, [])
  | Ctrl 'r' -> (enter_history_search model, [])
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
  | Tab -> (Completion_controller.handle_tab model, [])
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
