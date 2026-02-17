let prompt = "> "
let continued_prompt = ". "
let pending_prompt = "  "

type mode = Normal | Readline of string

let min_prompt_height = 5
let default_prompt_top term_height = max 2 (term_height - min_prompt_height + 1)
let clamp_prompt_top term_height row = max 2 (min row (default_prompt_top term_height))

type model = {
  lines : Unicode_string.t list;
  lex_cache : Syntax.Cache.t;
  (* Terminal coordinates - deprecated, will be removed *)
  cursor_row : int;
  cursor_col : int;
  (* Internal coordinates - source of truth *)
  cursor_line : int;  (* logical line index, 0-indexed *)
  cursor_pos : int;   (* grapheme position within line *)
  prompt_top_row : int;
  term_width : int;
  term_height : int;
  prompt_box_height : int;
  previous_prompt_top_row : int;
  previous_key : Tty_listener.key option;
  persistent_col : int;
  awaiting_response : bool;
  backend_response : Ffi_backend.response_chunk option;
  repl_output : Terminal_ops.span list option;
  repl_cursor : int * int;
  scroll_amount : int;
  history : History.t;
  flipping_through_history : int option;
  running_in_ide : bool;
  completion : Completion.t option;
  completion_dirty : bool;
  mode : mode;
}

type update_result =
  | Continue of model
  | Submit of string * model
  | Exit
  | Cancel

(* Line wrapping utilities - uses display width *)
let wrap_line width line = Unicode_string.wrap line ~width

let wrap_lines width lines = List.concat_map (wrap_line width) lines

(* Coordinate conversion between internal (line_idx, grapheme_col) and terminal (row, display_col) *)
let internal_to_terminal width lines (line_idx, col) =
  let rows_before =
    lines
    |> List.filteri (fun i _ -> i < line_idx)
    |> List.fold_left
         (fun acc line -> acc + List.length (wrap_line width line))
         0
  in
  let line = List.nth lines line_idx in
  let wrapped = wrap_line width line in
  (* Find which wrapped row contains grapheme col *)
  let rec find_row row_idx graphemes_so_far = function
    | [] -> (row_idx, 0)
    | chunk :: rest ->
        let chunk_len = Unicode_string.length chunk in
        let end_of_chunk = graphemes_so_far + chunk_len in
        (* Cursor is in this chunk if:
           - col is strictly before end, OR
           - col equals end AND this is the last chunk (no more rows to move to) *)
        if col < end_of_chunk || (col = end_of_chunk && rest = []) then
          let offset_in_chunk = col - graphemes_so_far in
          let display_col = Unicode_string.prefix_width chunk offset_in_chunk in
          (row_idx, display_col)
        else
          find_row (row_idx + 1) end_of_chunk rest
  in
  let row_in_line, col_in_line = find_row 0 0 wrapped in
  (rows_before + row_in_line, col_in_line)

let terminal_to_internal width lines (row, display_col) =
  let rec loop internal_row lines current_terminal_row =
    match lines with
    | [] -> failwith "terminal_to_internal: row out of bounds"
    | line :: rest ->
        let wrapped = wrap_line width line in
        let num_rows = List.length wrapped in
        let is_last_line = rest = [] in
        if
          row < current_terminal_row + num_rows
          || (is_last_line && row = current_terminal_row + num_rows)
        then
          let row_offset = row - current_terminal_row in
          (* Sum grapheme counts of previous wrapped rows *)
          let rec sum_graphemes acc idx l =
            if idx = 0 then acc
            else
              match l with
              | [] -> acc
              | h :: t -> sum_graphemes (acc + Unicode_string.length h) (idx - 1) t
          in
          let graphemes_before = sum_graphemes 0 row_offset wrapped in
          (* Convert display_col to grapheme offset within this wrapped row *)
          let chunk =
            if row_offset < num_rows then List.nth wrapped row_offset
            else Unicode_string.empty
          in
          let grapheme_col = Unicode_string.grapheme_at_width chunk display_col in
          (internal_row, graphemes_before + grapheme_col)
        else loop (internal_row + 1) rest (current_terminal_row + num_rows)
  in
  loop 0 lines 0

(* List helpers *)
let get_line lines line_idx = List.nth lines line_idx

let update_line lines line_idx new_line =
  List.mapi (fun i line -> if i = line_idx then new_line else line) lines

let effective_width model = model.term_width - String.length prompt
