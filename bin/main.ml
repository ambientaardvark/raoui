let prompt = "raoui> "

(* Terminal cursor helpers *)
let cursor_up n = if n = 0 then "" else Printf.sprintf "\x1b[%dA" n
let cursor_down n = if n = 0 then "" else Printf.sprintf "\x1b[%dB" n
let cursor_right n = if n = 0 then "" else Printf.sprintf "\x1b[%dC" n
let cursor_left n = if n = 0 then "" else Printf.sprintf "\x1b[%dD" n

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
  cursor_line : int;
  cursor_col : int;
  output : string option;
  prompt_height : int;
  term_width : int;
}

let init = { lines = [""]; cursor_line = 0; cursor_col = 0; output = None; prompt_height = 1; term_width = 80 }

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

(* Helpers *)
let current_line state = List.nth state.lines state.cursor_line

let update_current_line state new_line =
  let new_lines = List.mapi (fun i line ->
    if i = state.cursor_line then new_line else line
  ) state.lines in
  { state with lines = new_lines }

(* Update *)
let insert_char state c =
  let line = current_line state in
  let before = String.sub line 0 state.cursor_col in
  let after = String.sub line state.cursor_col (String.length line - state.cursor_col) in
  let new_line = before ^ String.make 1 c ^ after in
  { (update_current_line state new_line) with cursor_col = state.cursor_col + 1 }

let delete_char state =
  if state.cursor_col = 0 then
    if state.cursor_line = 0 then state
    else (
      (* Join with previous line *)
      print_string (cursor_up 1);
      let prev_line = List.nth state.lines (state.cursor_line - 1) in
      let curr_line = current_line state in
      let merged = prev_line ^ curr_line in
      let new_lines = state.lines
        |> List.filteri (fun i _ -> i <> state.cursor_line)
        |> List.mapi (fun i line -> if i = state.cursor_line - 1 then merged else line)
      in
      { state with lines = new_lines; cursor_line = state.cursor_line - 1; cursor_col = String.length prev_line }
    )
  else
    let line = current_line state in
    let before = String.sub line 0 (state.cursor_col - 1) in
    let after = String.sub line state.cursor_col (String.length line - state.cursor_col) in
    { (update_current_line state (before ^ after)) with cursor_col = state.cursor_col - 1 }


let insert_newline state =
  let line = current_line state in
  let before = String.sub line 0 state.cursor_col in
  let after = String.sub line state.cursor_col (String.length line - state.cursor_col) in
  let new_lines =
    List.concat_map (fun (i, l) ->
      if i = state.cursor_line then [before; after] else [l]
    ) (List.mapi (fun i l -> (i, l)) state.lines)
  in
  let prompt_width = String.length prompt in
  let effective_width = state.term_width - prompt_width in
  let (cursor_row, cursor_col) = internal_to_terminal effective_width state.lines (state.cursor_line, state.cursor_col) in
  [
    cursor_up cursor_row;
    cursor_left cursor_col;
    "\n";
    cursor_down cursor_row;
    cursor_right cursor_col
  ]
  |> String.concat ""
  |> print_string;
  { state with lines = new_lines; cursor_line = state.cursor_line + 1; cursor_col = 0; prompt_height = state.prompt_height + 1 }

let move_left state =
  if state.cursor_col > 0 then
    { state with cursor_col = state.cursor_col - 1 }
  else if state.cursor_line > 0 then
    let prev_line = List.nth state.lines (state.cursor_line - 1) in
    { state with cursor_line = state.cursor_line - 1; cursor_col = String.length prev_line }
  else
    state

let move_right state =
  let line = current_line state in
  if state.cursor_col < String.length line then
    { state with cursor_col = state.cursor_col + 1 }
  else if state.cursor_line < List.length state.lines - 1 then
    { state with cursor_line = state.cursor_line + 1; cursor_col = 0 }
  else
    state

let move_up state =
  let at_top_of_line = state.cursor_col < state.term_width in
  if at_top_of_line then
    if state.cursor_line = 0 then
      state
    else
      let last_line_len = (state.cursor_line - 1) |> List.nth state.lines |> String.length in
      print_string (cursor_up 1);
      { state with
        cursor_line = state.cursor_line - 1;
        cursor_col = (last_line_len / state.term_width) * state.term_width + state.cursor_col
      }
  else (
    print_string (cursor_up 1);
    { state with cursor_col = state.cursor_col - state.term_width}
  )

let move_down state =
  let line = current_line state in
  let at_bottom_of_line = state.cursor_col + state.term_width >= String.length line in
  if at_bottom_of_line then
    if state.cursor_line = List.length state.lines - 1 then
      state
    else
      let col_offset = state.cursor_col mod state.term_width in
      print_string (cursor_down 1);
      { state with
        cursor_line = state.cursor_line + 1;
        cursor_col = col_offset
      }
  else (
    print_string (cursor_down 1);
    { state with cursor_col = state.cursor_col + state.term_width }
  )

let submit state =
  let text = String.concat "\n" state.lines in
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
let go_to_start = "\r"
let clear_line = "\x1b[K"
let cursor_column col = Printf.sprintf "\x1b[%dG" col

let print_output text cursor_row total_rows =
  let lines_to_drop = cursor_down (total_rows - cursor_row + 5) in
  print_endline (go_to_start ^ lines_to_drop ^ text)

let view state =
  let prompt_width = String.length prompt in
  let effective_width = state.term_width - prompt_width in
  let wrapped = wrap_lines effective_width state.lines in
  let total_rows = List.length wrapped in
  let (cursor_row, cursor_col_in_wrap) =
    internal_to_terminal effective_width state.lines (state.cursor_line, state.cursor_col)
  in
  (match state.output with
   | Some text -> print_output text cursor_row total_rows
   | None -> ());
  (* Move to top of prompt area *)
  print_string (cursor_up cursor_row);
  print_string go_to_start;
  (* Print each wrapped line *)
  List.iteri (fun i line ->
    print_string clear_line;
    if i = 0 then print_string prompt
    else print_string (String.make prompt_width ' ');
    print_string line;
    if i < total_rows - 1 then print_string "\n"
  ) wrapped;
  (* Clear any leftover lines from previous render *)
  for _ = total_rows to state.prompt_height - 1 do
    print_string ("\n" ^ clear_line)
  done;
  (* Position cursor *)
  let rows_from_bottom = total_rows - 1 - cursor_row in
  print_string (cursor_up rows_from_bottom);
  print_string (cursor_column (prompt_width + cursor_col_in_wrap + 1));
  flush stdout;
  { state with prompt_height = total_rows }

(* Main loop *)
let run () =
  let rec loop state =
    let state = view state in
    match update (Raoui.Tty_listener.await_input ()) state with
    | None -> ()
    | Some state -> loop state
  in
  loop init

let () =
  let orig = set_raw_mode () in
  Fun.protect ~finally:(fun () -> print_newline (); restore_mode orig) run
