let prompt = "raoui> "

let set_raw_mode () =
  let termio = Unix.tcgetattr Unix.stdin in
  let raw_termio = { termio with
    Unix.c_icanon = false;
    Unix.c_echo = false;
    Unix.c_vmin = 1;
    Unix.c_vtime = 0;
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_termio;
  termio

let restore_mode termio =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

(* Model *)
type state = {
  lines : string list;
  cursor_row : int;       (* row within prompt box, 0 = first row *)
  cursor_col : int;       (* col within line, 0 = first char after prompt *)
  output : string option;
  prompt_top_row : int;   (* absolute terminal row of prompt box top, 1-indexed *)
  term_width : int;
  term_height : int;
}

(* Query cursor position using ANSI escape sequence *)
let get_cursor_position () =
  print_string "\x1b[6n";
  flush stdout;
  (* Response is ESC [ row ; col R *)
  let buf = Buffer.create 16 in
  let rec read_until_r () =
    let c = input_char stdin in
    if c = 'R' then ()
    else (Buffer.add_char buf c; read_until_r ())
  in
  read_until_r ();
  let response = Buffer.contents buf in
  (* Parse "ESC[row;col" *)
  Scanf.sscanf response "\x1b[%d;%d" (fun row col -> (row, col))

let make_init () =
  let (row, _col) = get_cursor_position () in
  let term_height = match Terminal_size.get_rows () with
    | Some h -> h
    | None -> failwith "can't get terminal height"
  in
  let term_width = match Terminal_size.get_columns () with
    | Some w -> w
    | None -> failwith "can't get terminal width"
  in
  { lines = [""]; cursor_row = 0; cursor_col = 0; output = None;
    prompt_top_row = row; term_width; term_height }

(* Wrapping and coordinate mapping *)
let wrap_line width line =
  let rec loop acc remaining =
    if String.length remaining < width then List.rev (remaining :: acc)
    else
      let chunk = String.sub remaining 0 width in
      let rest = String.sub remaining width (String.length remaining - width) in
      loop (chunk :: acc) rest
  in
  loop [] line

let wrap_lines width lines =
  List.concat_map (wrap_line width) lines

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
  let rec find_line line_idx rows_consumed remaining_lines =
    match remaining_lines with
    | [] -> failwith "terminal_to_internal: row out of bounds"
    | line :: rest ->
      let wrapped_count = List.length (wrap_line width line) in
      if row < rows_consumed + wrapped_count then
        (* cursor is within this line *)
        let row_within_line = row - rows_consumed in
        let col_within_line = row_within_line * width + col in
        (line_idx, col_within_line)
      else
        find_line (line_idx + 1) (rows_consumed + wrapped_count) rest
  in
  find_line 0 0 lines

(* Helpers *)
let get_line lines line_idx = List.nth lines line_idx

let update_line lines line_idx new_line =
  List.mapi (fun i line ->
    if i = line_idx then new_line else line
  ) lines

(* Update *)
let effective_width state = state.term_width - String.length prompt

let insert_char state c =
  let width = effective_width state in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  let line = get_line state.lines line_idx in
  let before = String.sub line 0 col in
  let after = String.sub line col (String.length line - col) in
  let new_line = before ^ String.make 1 c ^ after in
  let new_lines = update_line state.lines line_idx new_line in
  let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, col + 1) in
  { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let delete_char state =
  let width = effective_width state in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  if col = 0 then
    if line_idx = 0 then state
    else
      (* Join with previous line *)
      let prev_line = get_line state.lines (line_idx - 1) in
      let curr_line = get_line state.lines line_idx in
      let merged = prev_line ^ curr_line in
      let new_lines = state.lines
        |> List.filteri (fun i _ -> i <> line_idx)
        |> List.mapi (fun i line -> if i = line_idx - 1 then merged else line)
      in
      let (new_row, new_col) = internal_to_terminal width new_lines (line_idx - 1, String.length prev_line) in
      { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }
  else
    let line = get_line state.lines line_idx in
    let before = String.sub line 0 (col - 1) in
    let after = String.sub line col (String.length line - col) in
    let new_lines = update_line state.lines line_idx (before ^ after) in
    let (new_row, new_col) = internal_to_terminal width new_lines (line_idx, col - 1) in
    { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }


let insert_newline state =
  let width = effective_width state in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  let line = get_line state.lines line_idx in
  let before = String.sub line 0 col in
  let after = String.sub line col (String.length line - col) in
  let new_lines =
    List.concat_map (fun (i, l) ->
      if i = line_idx then [before; after] else [l]
    ) (List.mapi (fun i l -> (i, l)) state.lines)
  in
  let (new_row, new_col) = internal_to_terminal width new_lines (line_idx + 1, 0) in
  { state with lines = new_lines; cursor_row = new_row; cursor_col = new_col }

let move_left state =
  let width = effective_width state in
  if state.cursor_col > 0 then
    { state with cursor_col = state.cursor_col - 1 }
  else if state.cursor_row > 0 then
    { state with cursor_row = state.cursor_row - 1; cursor_col = width - 1 }
  else
    state

let move_right state =
  let width = effective_width state in
  let total_rows = List.length (wrap_lines width state.lines) in
  let (line_idx, col) = terminal_to_internal width state.lines (state.cursor_row, state.cursor_col) in
  let line = get_line state.lines line_idx in
  (* Check if we're at the end of the current logical line *)
  if col >= String.length line then
    if line_idx < List.length state.lines - 1 then
      (* Move to start of next line *)
      let (new_row, new_col) = internal_to_terminal width state.lines (line_idx + 1, 0) in
      { state with cursor_row = new_row; cursor_col = new_col }
    else
      state
  else if state.cursor_col < width - 1 then
    { state with cursor_col = state.cursor_col + 1 }
  else if state.cursor_row < total_rows - 1 then
    { state with cursor_row = state.cursor_row + 1; cursor_col = 0 }
  else
    state

let move_up state =
  let wrapped_lines = wrap_lines (effective_width state) state.lines in
  if state.cursor_row > 0 then
    { state with
      cursor_row = state.cursor_row - 1 ;
      cursor_col = min state.cursor_col (List.nth wrapped_lines (state.cursor_row - 1) |> String.length)
    }
  else
    state

let move_down state =
  let width = effective_width state in
  let wrapped_lines = wrap_lines width state.lines in
  let total_rows = List.length wrapped_lines in
  if state.cursor_row < total_rows - 1 then
    { state with
      cursor_row = state.cursor_row + 1;
      cursor_col = min state.cursor_col (List.nth wrapped_lines (state.cursor_row + 1) |> String.length)
    }
  else
    state

let submit state =
  let text = String.concat "\n" state.lines in
  let init = make_init () in
  { init with output = Some text }

let update key state =
  let open Raoui.Tty_listener in
  let width =
    match (Terminal_size.get_columns ()) with
    | Some w -> w
    | None -> failwith "can't get width"
  in
  let state = { state with output = None; term_width = width } in
  match key with
  | Ctrl 'd' -> None
  | Ctrl 'j' -> Some (submit state)  (* Ctrl+Enter to submit *)
  | Enter -> Some (insert_newline state)
  | Char c -> Some (insert_char state c)
  | Backspace -> Some (delete_char state)
  | Left -> Some (move_left state)
  | Right -> Some (move_right state)
  | Up -> Some (move_up state)
  | Down -> Some (move_down state)
  | _ -> Some state

(* View *)
let clear_line = "\x1b[K"
let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col

let view state =
  let prompt_width = String.length prompt in
  let width = effective_width state in
  let wrapped = wrap_lines width state.lines in
  let total_rows = List.length wrapped in
  (* Check if prompt would overflow terminal bottom, scroll if needed *)
  let prompt_bottom = state.prompt_top_row + total_rows - 1 in
  let overflow = prompt_bottom - state.term_height in
  let state =
    if overflow > 0 then (
      (* Scroll terminal by printing newlines at bottom *)
      print_string (cursor_to state.term_height 1);
      for _ = 1 to overflow do
        print_string "\n"
      done;
      { state with prompt_top_row = state.prompt_top_row - overflow }
    )
    else state
  in
  (* Compute viewport *)
  let viewport_start = max 1 state.prompt_top_row in
  let skip_rows = viewport_start - state.prompt_top_row in
  let visible_rows = List.filteri (fun i _ -> i >= skip_rows) wrapped in
  (* Handle output *)
  (match state.output with
   | Some text ->
     let output_row = state.prompt_top_row + total_rows in
     print_string (cursor_to output_row 1);
     print_endline text
   | None -> ());
  (* Move to top of visible area *)
  print_string (cursor_to viewport_start 1);
  (* Print each visible wrapped line *)
  List.iteri (fun i line ->
    print_string clear_line;
    if i + skip_rows = 0 then print_string prompt
    else print_string (String.make prompt_width ' ');
    print_string line;
    if i < List.length visible_rows - 1 then print_string "\n"
  ) visible_rows;
  (* Position cursor *)
  let cursor_abs_row = state.prompt_top_row + state.cursor_row in
  let cursor_abs_col = prompt_width + state.cursor_col + 1 in
  print_string (cursor_to cursor_abs_row cursor_abs_col);
  flush stdout;
  state

(* Main loop *)
let run () =
  let rec loop state =
    let state = view state in
    match update (Raoui.Tty_listener.await_input ()) state with
    | None -> ()
    | Some state -> loop state
  in
  loop (make_init ())

let () =
  let orig = set_raw_mode () in
  Fun.protect ~finally:(fun () -> print_newline (); restore_mode orig) run
