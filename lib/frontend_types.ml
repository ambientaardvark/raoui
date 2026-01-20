let prompt = "> "
let continued_prompt = ". "

type state = 
  { lines : string list
  ; cursor_row : int
  ; cursor_col : int
  ; prompt_top_row : int
  ; term_width : int
  ; term_height : int
  ; prompt_box_height : int
  ; previous_prompt_top_row : int
  ; previous_key : Tty_listener.key option
  ; persistent_col : int
  }

type update_result =
  | Continue of state
  | Submit of string
  | Exit

(* Line wrapping utilities *)
let wrap_line width line =
  let rec loop acc remaining =
    if String.length remaining < width then List.rev (remaining :: acc)
    else
      let chunk = String.sub remaining 0 width in
      let rest = String.sub remaining width (String.length remaining - width) in
      loop (chunk :: acc) rest
  in
  if width <= 0 then [line] else loop [] line

let wrap_lines width lines =
  List.concat_map (wrap_line width) lines

(* Coordinate conversion between internal (line_idx, col) and terminal (row, col) *)
let internal_to_terminal width lines (line_idx, col) =
  let rows_before =
    lines
    |> List.filteri (fun i _ -> i < line_idx)
    |> List.fold_left (fun acc line ->
         acc + List.length (wrap_line width line)) 0
  in
  let row_in_line = col / width in
  let col_in_line = col mod width in
  (rows_before + row_in_line, col_in_line)

let terminal_to_internal width lines (row, col) =
  let rec loop internal_row lines current_terminal_row =
    match lines with
    | [] -> failwith "terminal_to_internal: row out of bounds"
    | line :: rest ->
      let wrapped = wrap_line width line in
      let num_rows = List.length wrapped in
      let is_last_line = rest = [] in
      if row < current_terminal_row + num_rows || (is_last_line && row = current_terminal_row + num_rows) then
        let row_offset = row - current_terminal_row in
        let rec sum_lengths acc idx l =
          if idx = 0 then acc
          else match l with
            | [] -> acc
            | h :: t -> sum_lengths (acc + String.length h) (idx - 1) t
        in
        let chars_before = sum_lengths 0 row_offset wrapped in
        (internal_row, chars_before + col)
      else
        loop (internal_row + 1) rest (current_terminal_row + num_rows)
  in
  loop 0 lines 0

(* List helpers *)
let get_line lines line_idx = List.nth lines line_idx

let update_line lines line_idx new_line =
  List.mapi (fun i line ->
    if i = line_idx then new_line else line
  ) lines

let effective_width state = state.term_width - String.length prompt
