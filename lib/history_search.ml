open Frontend_types

(* Design:
   - History_search s stores the search input (what the user types)
   - model.input.lines stores the search result from history
   - cursor_pos tracks position within the search input
   - The view renders "r-search: {input}> " as the prompt, result as content *)

let get_input model =
  match model.input.mode with History_search s -> s | _ -> assert false

let find_match model =
  let input_str = get_input model |> Unicode_string.to_string in
  if String.length input_str = 0 then None
  else History.search_history model.input.history ("%" ^ input_str ^ "%")

let search_and_update model =
  let lines =
    match find_match model with
    | None -> [ Unicode_string.empty ]
    | Some (_mode, text) ->
        text |> String.split_on_char '\n'
        |> List.map (fun s -> Unicode_string.of_string s |> Result.get_ok)
  in
  {
    model with
    input = { model.input with lines; lex_cache = R_lex_cache.create lines };
  }

let cancel model =
  {
    model with
    input =
      {
        model.input with
        mode = Normal;
        lines = [ Unicode_string.empty ];
        lex_cache = R_lex_cache.create [ Unicode_string.empty ];
        cursor_line = 0;
        cursor_pos = 0;
      };
  }

let submit model =
  (* Adopt the matched entry's input mode (r/shell/ai), like arrow-key recall. *)
  let mode =
    match find_match model with
    | Some (mode_string, _text) -> Mode_common.mode_of_history_string mode_string
    | None -> Normal
  in
  let last_line_idx = List.length model.input.lines - 1 in
  let last_line = List.nth model.input.lines last_line_idx in
  let new_pos = Unicode_string.length last_line in
  {
    model with
    input =
      {
        model.input with
        mode;
        lex_cache = R_lex_cache.create model.input.lines;
        cursor_line = last_line_idx;
        cursor_pos = new_pos;
      };
  }

let insert_char model c =
  let search = get_input model in
  match Unicode_string.insert_string search ~pos:model.input.cursor_pos c with
  | Error _ -> model
  | Ok new_search ->
      {
        model with
        input =
          {
            model.input with
            mode = History_search new_search;
            cursor_pos = model.input.cursor_pos + 1;
          };
      }

let delete_char model =
  if model.input.cursor_pos = 0 then model
  else
    let search = get_input model in
    let new_search =
      Unicode_string.delete search (model.input.cursor_pos - 1)
    in
    {
      model with
      input =
        {
          model.input with
          mode = History_search new_search;
          cursor_pos = model.input.cursor_pos - 1;
        };
    }

let delete_char_after model =
  let search = get_input model in
  if model.input.cursor_pos >= Unicode_string.length search then model
  else
    let new_search = Unicode_string.delete search model.input.cursor_pos in
    { model with input = { model.input with mode = History_search new_search } }

let delete_before_cursor model =
  let search = get_input model in
  let new_search =
    Unicode_string.delete_range search ~start:0 ~len:model.input.cursor_pos
  in
  {
    model with
    input =
      { model.input with mode = History_search new_search; cursor_pos = 0 };
  }

let move_left model =
  if model.input.cursor_pos > 0 then
    {
      model with
      input = { model.input with cursor_pos = model.input.cursor_pos - 1 };
    }
  else model

let move_right model =
  let search = get_input model in
  if model.input.cursor_pos < Unicode_string.length search then
    {
      model with
      input = { model.input with cursor_pos = model.input.cursor_pos + 1 };
    }
  else model

let go_to_start model =
  { model with input = { model.input with cursor_pos = 0 } }

let go_to_end model =
  let search = get_input model in
  let len = Unicode_string.length search in
  { model with input = { model.input with cursor_pos = len } }

let is_word_char s =
  if String.length s > 1 then true
  else
    match String.get s 0 with
    | ' ' | '\t' | '/' | ',' | '=' | '-' | '+' | '[' | ']' | '{' | '}' | '('
    | ')' | '|' | '\\' | '?' | '<' | '>' | '`' | '~' | '!' | '@' | '#' | '$'
    | '%' | '^' | '&' | '*' | ';' | ':' | '\'' | '"' ->
        false
    | _ -> true

let go_to_next_word model =
  let search = get_input model in
  let len = Unicode_string.length search in
  let rec loop seen_word pos =
    if pos >= len then pos
    else
      let c = Unicode_string.cluster_at search pos in
      if is_word_char c then loop true (pos + 1)
      else if seen_word then pos
      else loop false (pos + 1)
  in
  let new_pos = loop false model.input.cursor_pos in
  { model with input = { model.input with cursor_pos = new_pos } }

let go_to_last_word model =
  let search = get_input model in
  let rec loop seen_word pos =
    if pos <= 0 then 0
    else
      let c = Unicode_string.cluster_at search (pos - 1) in
      if is_word_char c then loop true (pos - 1)
      else if seen_word then pos
      else loop false (pos - 1)
  in
  let new_pos = loop false model.input.cursor_pos in
  { model with input = { model.input with cursor_pos = new_pos } }

let insert_paste model text =
  let search = get_input model in
  let clean = String.concat "" (String.split_on_char '\n' text) in
  match
    Unicode_string.insert_string search ~pos:model.input.cursor_pos clean
  with
  | Error _ -> model
  | Ok new_search ->
      let added =
        Unicode_string.length new_search - Unicode_string.length search
      in
      {
        model with
        input =
          {
            model.input with
            mode = History_search new_search;
            cursor_pos = model.input.cursor_pos + added;
          };
      }

let apply_key key model =
  let open Tty_listener in
  let m =
    match key with
    | Ctrl 'c' | Escape -> cancel model
    | Enter -> submit model
    | Up | Down | Ctrl 'p' -> model
    | Ctrl 'd' ->
        let search = get_input model in
        if Unicode_string.is_empty search then cancel model
        else delete_char_after model |> search_and_update
    | Ctrl 'u' -> delete_before_cursor model |> search_and_update
    | Ctrl 'a' -> go_to_start model
    | Ctrl 'e' -> go_to_end model
    | Other "next word" -> go_to_next_word model
    | Other "last word" -> go_to_last_word model
    | Char c -> insert_char model c |> search_and_update
    | Backspace ->
        let search = get_input model in
        if Unicode_string.is_empty search then cancel model
        else delete_char model |> search_and_update
    | Left -> move_left model
    | Right -> move_right model
    | Paste text -> insert_paste model text |> search_and_update
    | _ -> model
  in
  (m, [])
