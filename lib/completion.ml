(** Completion state management *)

type t = {
  items : string list;          (** All items from backend *)
  filtered : string list;       (** Items matching current prefix *)
  selected : int;               (** -1 = dropdown only, 0+ = completion mode *)
  token_start : int;            (** Grapheme position where token starts *)
  original_token : string;      (** Saved token for Escape revert *)
}

let create ~token_start items =
  {
    items;
    filtered = items;
    selected = -1;
    token_start;
    original_token = "";
  }

let filter t ~prefix =
  let filtered =
    List.filter
      (fun item ->
        String.length item >= String.length prefix
        && String.sub item 0 (String.length prefix) = prefix)
      t.items
  in
  if filtered = [] then None else Some { t with filtered }

let cycle_next t =
  if List.length t.filtered = 0 then t
  else
    let new_selected =
      if t.selected < 0 then 0 else (t.selected + 1) mod List.length t.filtered
    in
    { t with selected = new_selected }

let current_completion t =
  if t.selected < 0 || t.selected >= List.length t.filtered then None
  else Some (List.nth t.filtered t.selected)

let is_in_completion_mode t = t.selected >= 0

let token_start t = t.token_start

let save_original_token t ~token = { t with original_token = token }

let original_token t = t.original_token

let dropdown_size t = min 5 (List.length t.filtered)

let filtered_items t = t.filtered

let selected_index t = t.selected
