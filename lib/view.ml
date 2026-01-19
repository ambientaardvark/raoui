open Frontend_types

let log message =
  let oc = open_out_gen [Open_append; Open_creat] 0o666 "debug_log.txt" in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> Printf.fprintf oc "%s\n" message)

let clear_line = "\x1b[K"
let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col

let scroll_up buf n = Printf.bprintf buf "\x1b[%dS" n
let scroll_down buf n = Printf.bprintf buf "\x1b[%dT" n

let scroll_terminal buf n =
  if n = 0 then ()
  else if n < 0 then scroll_up buf (-n)
  else scroll_down buf n

let view state =
  let buf = Buffer.create 1024 in
  let prompt_width = String.length prompt in
  let width = effective_width state in
  let wrapped = wrap_lines width state.lines in
  let total_rows = List.length wrapped in

  let _log_string_list li  = 
    let rec loop b li = 
      match li with | [] -> b | hd::tl -> loop (b ^ ", " ^ hd) tl 
    in
    log (loop "" li)
  in
  (* log_string_list wrapped; *)

  scroll_terminal buf (state.prompt_top_row - state.previous_prompt_top_row);

  let viewport_start = max 1 state.prompt_top_row in
  Buffer.add_string buf (cursor_to viewport_start 1);
  let skip_rows = viewport_start - state.prompt_top_row in
  List.iteri (fun i line ->
    if i >= skip_rows && i < skip_rows + state.term_height then begin
      Buffer.add_string buf clear_line;
      (if i = 0 then Buffer.add_string buf prompt else Buffer.add_string buf continued_prompt);
      Buffer.add_string buf line;
      if i < skip_rows + state.term_height - 1 && i < total_rows - 1 then Buffer.add_char buf '\n'
    end
  ) wrapped;

  log (Printf.sprintf "skip rows: %d, viewport_start: %d, prompt_top_row: %d, total_rows: %d"
    skip_rows viewport_start state.prompt_top_row total_rows);

  let visible_rows = min (total_rows - skip_rows) state.term_height in
  let old_visible_rows = min state.prompt_box_height state.term_height in
  let extra_lines = old_visible_rows - visible_rows in
  let cursor_after_render = viewport_start + visible_rows - 1 in
  let possible_space = max 0 (state.term_height - cursor_after_render) in
  let rows_to_clear = max 0 (min extra_lines possible_space) in
  List.init rows_to_clear (fun _ -> "\n" ^ clear_line)
  |> String.concat ""
  |> Buffer.add_string buf;

  (* Position cursor *)
  let cursor_abs_row = state.prompt_top_row + state.cursor_row in
  let cursor_abs_col = prompt_width + state.cursor_col + 1 in
  Buffer.add_string buf (cursor_to cursor_abs_row cursor_abs_col);

  Buffer.contents buf
