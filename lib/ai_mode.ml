open Frontend_types
open Text_editor

(* Client-side commands intercepted before anything is sent to the AI. *)
let reset_commands = [ "/new"; "/reset"; "/clear" ]

let submit model =
  let text = Unicode_string.to_string (current_line model) in
  let cleared =
    Mode_common.clear_model_for_submit
      { model with input = { model.input with mode = Normal } }
  in
  if List.mem (String.trim text) reset_commands then
    (cleared, [ Repl_effect.ResetAiSession ])
  else begin
    History.add_to_history ~mode:"ai" model.input.history model.input.lines;
    (cleared, [ Repl_effect.SubmitAiQuery text ])
  end

let apply_key key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' -> (Mode_common.set_mode_normal_blank model, [])
  | Ctrl 'p' | Up | Down -> (model, [])
  | Enter -> submit model
  | Ctrl 'd' ->
      if is_empty_input model then (Mode_common.set_mode_normal_blank model, [])
      else (delete_char_after_cursor model, [])
  | Ctrl 'u' -> (delete_before_cursor model, [])
  | Ctrl 'a' -> (go_to_line_start model, [])
  | Ctrl 'e' -> (go_to_line_end model, [])
  | Other "next word" -> (go_to_next_word model, [])
  | Other "last word" -> (go_to_last_word model, [])
  | Char c -> (user_input_char model c, [])
  (* Backspace on empty input returns to normal mode (Julia-style). *)
  | Backspace when prompt_is_empty model ->
      (Mode_common.set_mode_normal_blank model, [])
  | Backspace -> (user_input_delete model, [])
  | Left -> (move_left model, [])
  | Right -> (move_right model, [])
  | Paste text -> (insert_paste model text, [])
  | _ -> (model, [])
