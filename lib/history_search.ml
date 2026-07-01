open Frontend_types

(* Design (fish-style pager):
   - History_search carries the query, the matching entries, and the selection.
   - model.input.lines stays blank while searching; matches render in a list
     under the query row (see view_search_pager in view.ml).
   - cursor_pos tracks position within the query.
   - Up/Down move the selection; Enter inserts the selected entry into the
     prompt and switches to the mode it was submitted from. *)

let max_matches = 50

let get_state model =
  match model.input.mode with History_search s -> s | _ -> assert false

let get_input model = (get_state model).search

let fresh_state history search =
  let query = Unicode_string.to_string search in
  {
    search;
    matches = History.search_matches history query ~limit:max_matches;
    selected = 0;
  }

(* Enter search mode from any input mode. The input buffer goes blank: the
   query lives in the mode's search_state, and an empty query matches
   everything so the pager opens showing the most recent entries. *)
let enter model =
  {
    model with
    input =
      {
        model.input with
        mode =
          History_search (fresh_state model.input.history Unicode_string.empty);
        lines = [ Unicode_string.empty ];
        lex_cache = R_lex_cache.create [ Unicode_string.empty ];
        cursor_pos = 0;
        cursor_line = 0;
        completion = None;
      };
  }

(* Re-run the search for the current query; any edit resets the selection. *)
let search_and_update model =
  let state = fresh_state model.input.history (get_input model) in
  { model with input = { model.input with mode = History_search state } }

let set_search model new_search =
  {
    model with
    input =
      {
        model.input with
        mode = History_search { (get_state model) with search = new_search };
      };
  }

let move_selection model ~amount =
  let state = get_state model in
  let n = List.length state.matches in
  if n = 0 then model
  else
    let selected = max 0 (min (n - 1) (state.selected + amount)) in
    {
      model with
      input =
        { model.input with mode = History_search { state with selected } };
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

(* Insert the selected entry into the prompt, adopting the input mode it was
   submitted from (r/shell/ai), like arrow-key recall. *)
let submit model =
  let state = get_state model in
  match List.nth_opt state.matches state.selected with
  | None -> cancel model
  | Some (mode_string, text) ->
      let lines =
        String.split_on_char '\n' text
        |> List.map (fun s -> Unicode_string.of_string s |> Result.get_ok)
      in
      let last_line_idx = List.length lines - 1 in
      let last_line = List.nth lines last_line_idx in
      {
        model with
        input =
          {
            model.input with
            mode = Mode_common.mode_of_history_string mode_string;
            lines;
            lex_cache = R_lex_cache.create lines;
            cursor_line = last_line_idx;
            cursor_pos = Unicode_string.length last_line;
          };
      }

let insert_char model c =
  let search = get_input model in
  match Unicode_string.insert_string search ~pos:model.input.cursor_pos c with
  | Error _ -> model
  | Ok new_search ->
      let model = set_search model new_search in
      {
        model with
        input = { model.input with cursor_pos = model.input.cursor_pos + 1 };
      }

let delete_char model =
  if model.input.cursor_pos = 0 then model
  else
    let search = get_input model in
    let new_search =
      Unicode_string.delete search (model.input.cursor_pos - 1)
    in
    let model = set_search model new_search in
    {
      model with
      input = { model.input with cursor_pos = model.input.cursor_pos - 1 };
    }

let delete_char_after model =
  let search = get_input model in
  if model.input.cursor_pos >= Unicode_string.length search then model
  else set_search model (Unicode_string.delete search model.input.cursor_pos)

let delete_before_cursor model =
  let search = get_input model in
  let new_search =
    Unicode_string.delete_range search ~start:0 ~len:model.input.cursor_pos
  in
  let model = set_search model new_search in
  { model with input = { model.input with cursor_pos = 0 } }

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
      let model' = set_search model new_search in
      {
        model' with
        input =
          { model'.input with cursor_pos = model.input.cursor_pos + added };
      }

let apply_key key model =
  let open Tty_listener in
  let m =
    match key with
    | Ctrl 'c' | Escape -> cancel model
    | Enter -> submit model
    | Up | Ctrl 'p' -> move_selection model ~amount:(-1)
    | Down | Ctrl 'r' -> move_selection model ~amount:1
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
