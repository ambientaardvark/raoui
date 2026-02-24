open Frontend_types
open Tty_listener

let set_mode_normal_blank model =
  { model with
    mode = Frontend_types.Normal;
    lines = [ Unicode_string.empty ];
    lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
  }

let submit model =
  { model with mode = Normal }

let get_input_str model =
  match model.mode with
  | History_search s -> s
  | _ -> failwith "Should be in history mode when accessing history search"

let update_lines model =
  let input_str = model |> get_input_str |> Unicode_string.to_string in
  let search_result = History.search_history model.history input_str in
  let lines =
    search_result
    |> String.split_on_char '\n'
    |> List.map Unicode_string.of_string
    |> List.map Result.get_ok
  in
  { model with lines }

let main_key_logic model key = function
  | Ctrl 'd' -> (
    let input = match model.mode with History_search s -> s | _ -> assert false in
    if Unicode_string.is_empty input
    then submit { model with mode = History_search Unicode_string.empty }
    else (delete_char_after_cursor model))
  | Ctrl 'u' -> (delete_before_cursor model)
  | Ctrl 'a' -> (go_to_line_start model)
  | Ctrl 'e' -> (go_to_line_end model)
  | Other "next word" -> (go_to_next_word model)
  | Other "last word" -> (go_to_last_word model)
  | Char c -> (user_input_char model c)
  | Backspace -> (user_input_delete model)
  | Left -> (move_left model)
  | Right -> (move_right model)
  | Paste text -> (insert_paste model text)
  | _ -> model

let apply_key key model =
  match key with
  | Ctrl 'c' ->
      (set_mode_normal_blank model)
  | Ctrl 'p' | Up | Down ->
      (* No history navigation in readline mode *)
      model
  | Enter ->
      (* Enter always submits in readline mode *)
      submit model
  | _ ->
      (* All other editing keys work normally *)
      let model = match key with
        | Tab | Escape -> model
        | _ -> (match model.completion with
            | Some cs when Completion.is_in_completion_mode cs ->
                { model with completion = None }
            | _ -> model)
      in
      main_key_logic model key
      |> update_lines model
      |> Continue
