open Frontend_types
open Text_editor

let submit model =
  let text = Unicode_string.to_string (current_line model) in
  let new_model =
    {
      model with
      mode = Frontend_types.Normal;
      completion = None;
      lines = [ Unicode_string.empty ];
      lex_cache = R_lex_cache.create [ Unicode_string.empty ];
      cursor_row = 0;
      cursor_col = 0;
      cursor_line = 0;
      cursor_pos = 0;
    }
  in
  (new_model, [ Repl_effect.SubmitReadlineInput text ])

let apply_key key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' ->
      (Mode_common.set_mode_normal_blank model, [ Repl_effect.SubmitReadlineInput "" ])
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
