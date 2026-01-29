open Base
open Frontend_types

module Term = Terminal_ops.Make(struct let term_type = "ansi" end)

let log message =
  let oc = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 "debug_log.txt" in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.Printf.fprintf oc "%s\n" message)

let view_ops model =
  let open Terminal_ops in
  let ops = Queue.create () in
  let add op = Queue.enqueue ops op in

  let prompt_width = String.length prompt in
  let width = effective_width model in
  let wrapped = wrap_lines width model.lines in
  let total_rows = List.length wrapped in

  add (if model.awaiting_response then Hide_cursor else Show_cursor);

  if model.scroll_amount < 0 then add (Scroll_up (-model.scroll_amount))
  else if model.scroll_amount > 0 then add (Scroll_down model.scroll_amount);

  let viewport_start = max 1 model.prompt_top_row in
  add (Cursor_to (viewport_start, 1));
  let skip_rows = viewport_start - model.prompt_top_row in
  List.iteri wrapped ~f:(fun i line ->
    if i >= skip_rows && i < skip_rows + model.term_height then begin
      add Clear_to_eol;
      let p = match i, model.awaiting_response with
        | 0, false -> prompt
        | 0, true -> pending_prompt
        | _ -> continued_prompt
      in
      add (Print [(`Accent, p); (`Plain, Unicode_string.to_string line)]);
      if i < skip_rows + model.term_height - 1 && i < total_rows - 1 then add Newline
    end
  );

  let visible_rows = min (total_rows - skip_rows) model.term_height in
  let old_visible_rows = min model.prompt_box_height model.term_height in
  let extra_lines = old_visible_rows - visible_rows in
  let cursor_after_render = viewport_start + visible_rows - 1 in
  let possible_space = max 0 (model.term_height - cursor_after_render) in
  let rows_to_clear = max 0 (min extra_lines possible_space) in
  for _ = 1 to rows_to_clear do
    add Newline;
    add Clear_to_eol
  done;

  (* Position cursor *)
  let cursor_abs_row = model.prompt_top_row + model.cursor_row in
  let cursor_abs_col = prompt_width + model.cursor_col + 1 in
  add (Cursor_to (cursor_abs_row, cursor_abs_col));

  ops

let view model =
  Term.render (view_ops model)
