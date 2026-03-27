(** Completion state management *)

type item_kind =
  | Backend
  | Backslash of Backslash_command.command

type item = {
  label : string;
  kind : item_kind;
}

type t = {
  items : item list;            (** All items from backend *)
  filtered : item list;         (** Items matching current prefix *)
  selected : int option;        (** Current selected index, implicit or explicit *)
  explicit_selection : bool;    (** True once user starts cycling with Tab *)
  token_start : int;            (** Grapheme position where token starts *)
  original_token : string;      (** Saved token for Escape revert *)
}

let max_visible_rows = 4

let backend_item label = { label; kind = Backend }
let backslash_item command = { label = Backslash_command.label command; kind = Backslash command }
let label item = item.label
let kind item = item.kind

let create ~token_start items =
  {
    items;
    filtered = items;
    selected = None;
    explicit_selection = false;
    token_start;
    original_token = "";
  }

let infer_selected_index filtered ~prefix =
  List.find_mapi
    (fun idx item -> if String.equal item.label prefix then Some idx else None)
    filtered

let effective_selected_index t = t.selected

let filter t ~prefix =
  let filtered =
    List.filter
      (fun item ->
        String.length item.label >= String.length prefix
        && String.sub item.label 0 (String.length prefix) = prefix)
      t.items
  in
  if filtered = [] then None
  else
    let selected =
      if t.explicit_selection then t.selected
      else infer_selected_index filtered ~prefix
    in
    Some { t with filtered; selected }

let cycle_next t =
  if List.length t.filtered = 0 then t
  else
    let new_selected =
      match effective_selected_index t with
      | None -> 0
      | Some idx -> (idx + 1) mod List.length t.filtered
    in
    { t with selected = Some new_selected; explicit_selection = true }

let current_completion t =
  match effective_selected_index t with
  | Some idx when idx >= 0 && idx < List.length t.filtered ->
      Some (List.nth t.filtered idx)
  | _ -> None

let is_in_completion_mode t = t.explicit_selection

let token_start t = t.token_start

let save_original_token t ~token = { t with original_token = token }

let original_token t = t.original_token

let dropdown_size t = min max_visible_rows (List.length t.filtered)

let filtered_items t = t.filtered

let visible_window_start t =
  let len = List.length t.filtered in
  match effective_selected_index t with
  | None -> 0
  | Some idx ->
      if len <= max_visible_rows then 0
      else
        let desired_start = idx - (max_visible_rows - 2) in
        max 0 (min desired_start (len - max_visible_rows))

let visible_items t =
  let start = visible_window_start t in
  t.filtered
  |> List.filteri (fun idx _ -> idx >= start && idx < start + max_visible_rows)

let selected_index_in_window t =
  match effective_selected_index t with
  | None -> None
  | Some selected ->
      let start = visible_window_start t in
      let idx = selected - start in
      if idx < 0 || idx >= dropdown_size t then None else Some idx

let selected_index t = effective_selected_index t
