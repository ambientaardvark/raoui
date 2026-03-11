open Frontend_types

(* Design:
   - History_search s stores the search input (what the user types)
   - model.lines stores the search result from history
   - cursor_pos/cursor_col track position within the search input
   - The view renders "r-search: {input}> " as the prompt, result as content *)

let get_input model =
  match model.mode with
  | History_search s -> s
  | _ -> assert false

let search_and_update model =
  let input_str = get_input model |> Unicode_string.to_string in
  if String.length input_str = 0 then
    { model with
      lines = [ Unicode_string.empty ];
      lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
    }
  else
    let pattern = "%" ^ input_str ^ "%" in
    let search_result = History.search_history model.history pattern in
    if String.length search_result = 0 then
      { model with
        lines = [ Unicode_string.empty ];
        lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
      }
    else
      let lines =
        search_result
        |> String.split_on_char '\n'
        |> List.map (fun s -> Unicode_string.of_string s |> Result.get_ok)
      in
      { model with lines; lex_cache = Syntax.Cache.create lines }

let sync_cursor_col model =
  let search = get_input model in
  let col = Unicode_string.prefix_width search model.cursor_pos in
  { model with cursor_col = col; cursor_row = 0; cursor_line = 0 }

let cancel model =
  { model with
    mode = Normal;
    lines = [ Unicode_string.empty ];
    lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
  }

let submit model =
  let last_line_idx = List.length model.lines - 1 in
  let last_line = List.nth model.lines last_line_idx in
  let new_pos = Unicode_string.length last_line in
  let width = effective_width model in
  let new_row, new_col =
    internal_to_terminal width model.lines (last_line_idx, new_pos)
  in
  { model with
    mode = Normal;
    lex_cache = Syntax.Cache.create model.lines;
    cursor_line = last_line_idx;
    cursor_pos = new_pos;
    cursor_row = new_row;
    cursor_col = new_col;
  }

let insert_char model c =
  let search = get_input model in
  match Unicode_string.insert_string search ~pos:model.cursor_pos c with
  | Error _ -> model
  | Ok new_search ->
    { model with
      mode = History_search new_search;
      cursor_pos = model.cursor_pos + 1;
    }
    |> sync_cursor_col

let delete_char model =
  if model.cursor_pos = 0 then model
  else
    let search = get_input model in
    let new_search = Unicode_string.delete search (model.cursor_pos - 1) in
    { model with
      mode = History_search new_search;
      cursor_pos = model.cursor_pos - 1;
    }
    |> sync_cursor_col

let delete_char_after model =
  let search = get_input model in
  if model.cursor_pos >= Unicode_string.length search then model
  else
    let new_search = Unicode_string.delete search model.cursor_pos in
    { model with mode = History_search new_search }
    |> sync_cursor_col

let delete_before_cursor model =
  let search = get_input model in
  let new_search =
    Unicode_string.delete_range search ~start:0 ~len:model.cursor_pos
  in
  { model with
    mode = History_search new_search;
    cursor_pos = 0;
    cursor_col = 0;
  }

let move_left model =
  if model.cursor_pos > 0 then
    { model with cursor_pos = model.cursor_pos - 1 }
    |> sync_cursor_col
  else model

let move_right model =
  let search = get_input model in
  if model.cursor_pos < Unicode_string.length search then
    { model with cursor_pos = model.cursor_pos + 1 }
    |> sync_cursor_col
  else model

let go_to_start model =
  { model with cursor_pos = 0; cursor_col = 0; cursor_row = 0 }

let go_to_end model =
  let search = get_input model in
  let len = Unicode_string.length search in
  { model with cursor_pos = len }
  |> sync_cursor_col

let is_word_char s =
  if String.length s > 1 then true
  else
    match String.get s 0 with
    | ' ' | '\t' | '/' | ',' | '=' | '-' | '+' | '[' | ']' | '{' | '}'
    | '(' | ')' | '|' | '\\' | '?' | '<' | '>' | '`' | '~' | '!' | '@'
    | '#' | '$' | '%' | '^' | '&' | '*' | ';' | ':' | '\'' | '"' ->
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
  let new_pos = loop false model.cursor_pos in
  { model with cursor_pos = new_pos } |> sync_cursor_col

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
  let new_pos = loop false model.cursor_pos in
  { model with cursor_pos = new_pos } |> sync_cursor_col

let insert_paste model text =
  let search = get_input model in
  let clean = String.concat "" (String.split_on_char '\n' text) in
  match Unicode_string.insert_string search ~pos:model.cursor_pos clean with
  | Error _ -> model
  | Ok new_search ->
    let added = Unicode_string.length new_search - Unicode_string.length search in
    { model with
      mode = History_search new_search;
      cursor_pos = model.cursor_pos + added;
    }
    |> sync_cursor_col

let apply_key key model =
  let open Tty_listener in
  let m = match key with
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
