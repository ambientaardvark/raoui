open Frontend_types

module Term = Terminal_ops.Make(struct let term_type = "ansi" end)

let log message =
  let oc = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 "/Users/alanlee/Documents/Programs/raoui/debug_log.txt" in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.Printf.fprintf oc "%s\n" message)

let view_ops model =
  let open Terminal_ops in
  let ops = Queue.create () in
  let add op = Queue.add op ops in

  let prompt_width = String.length prompt in
  let width = effective_width model in

  (* Convert cached tokens to spans *)
  let highlighted_lines =
    List.map (fun lex_line -> Syntax.tokens_to_spans lex_line.tokens) model.lex_cache
  in

  (* Wrap each line's spans for display *)
  let wrapped_with_spans =
    List.concat_map (fun spans ->
      (* For now, treat each logical line as one wrapped row *)
      (* TODO: proper wrapping that preserves span boundaries *)
      [spans]
    ) highlighted_lines
  in
  (* Also need wrapped lines for cursor positioning *)
  let wrapped = wrap_lines width model.lines in
  let total_rows = List.length wrapped in

  add (if model.awaiting_response then Hide_cursor else Show_cursor);

  if model.scroll_amount < 0 then add (Scroll_up (-model.scroll_amount))
  else if model.scroll_amount > 0 then add (Scroll_down model.scroll_amount);

  let viewport_start = max 1 model.prompt_top_row in
  add (Cursor_to (viewport_start, 1));
  let skip_rows = viewport_start - model.prompt_top_row in

  (* Map wrapped row index to logical line index *)
  let row_to_line_idx =
    let rec build acc line_idx = function
      | [] -> List.rev acc
      | line :: rest ->
          let num_wrapped = List.length (wrap_line width line) in
          let entries = List.init num_wrapped (fun _ -> line_idx) in
          build (List.rev_append entries acc) (line_idx + 1) rest
    in
    build [] 0 model.lines |> Array.of_list
  in

  List.iteri (fun i line ->
    if i >= skip_rows && i < skip_rows + model.term_height then begin
      add Clear_to_eol;
      let p = match i, model.awaiting_response with
        | 0, false -> prompt
        | 0, true -> pending_prompt
        | _ -> continued_prompt
      in
      (* Get the highlighted spans for this row's logical line *)
      let line_idx = if i < Array.length row_to_line_idx then row_to_line_idx.(i) else 0 in
      let spans =
        match List.nth_opt wrapped_with_spans line_idx with
        | Some spans -> spans
        | None -> [ (`Plain, Unicode_string.to_string line) ]
      in
      (* For wrapped lines, we need to slice the spans - for now just use the line content *)
      (* TODO: proper span slicing for wrapped lines *)
      let content =
        match List.nth_opt model.lines line_idx with
        | Some model_line when List.length (wrap_line width model_line) > 1 ->
            (* Multi-wrap line: fall back to plain text for this wrapped segment *)
            [ (`Plain, Unicode_string.to_string line) ]
        | _ -> spans
      in
      add (Print ((`Accent, p) :: content));
      if i < skip_rows + model.term_height - 1 && i < total_rows - 1 then add Newline
    end
  ) wrapped;

  let visible_rows = max 0 (min (total_rows - skip_rows) model.term_height) in

  (* Render completion dropdown *)
  let dropdown_rows =
    match model.completion with
    | None -> 0
    | Some cs when cs.filtered = [] -> 0
    | Some cs ->
        let prompt_end_row = viewport_start + visible_rows - 1 in
        let available = model.term_height - prompt_end_row in
        let max_items = min 5 (max 0 available) in
        let num_items = min max_items (List.length cs.filtered) in
        if num_items = 0 then 0
        else begin
          let line =
            match List.nth_opt model.lines model.cursor_line with
            | Some line -> line
            | None -> Unicode_string.empty
          in
          let col_offset =
            prompt_width + Unicode_string.prefix_width line cs.token_start + 1
          in
          (* Scroll window to keep selected item visible *)
          let start_idx =
            if cs.selected < 0 then 0
            else
              let max_start = max 0 (List.length cs.filtered - num_items) in
              min max_start (max 0 (cs.selected - num_items / 2))
          in
          for i = 0 to num_items - 1 do
            let idx = start_idx + i in
            add Newline;
            add Clear_to_eol;
            let item =
              match List.nth_opt cs.filtered idx with
              | Some item -> item
              | None -> ""
            in
            let padding = String.make (max 0 (col_offset - 1)) ' ' in
            let style =
              if idx = cs.selected then `Completion_selected
              else `Completion
            in
            add (Print [(style, padding ^ item)])
          done;
          num_items
        end
  in

  let total_box_rows = visible_rows + dropdown_rows in
  let old_visible_rows = min model.prompt_box_height model.term_height in
  let extra_lines = old_visible_rows - total_box_rows in
  let cursor_after_render = viewport_start + total_box_rows - 1 in
  let possible_space = max 0 (model.term_height - cursor_after_render) in
  let rows_to_clear = max 0 (min extra_lines possible_space) in
  for _ = 1 to rows_to_clear do
    add Newline;
    add Clear_to_eol
  done;

  (* Position cursor: in output area during eval, in prompt otherwise *)
  if model.awaiting_response then begin
    let row, col = model.repl_cursor in
    add (Cursor_to (row, col))
  end else begin
    let cursor_abs_row = model.prompt_top_row + model.cursor_row in
    let cursor_abs_col = prompt_width + model.cursor_col + 1 in
    add (Cursor_to (cursor_abs_row, cursor_abs_col))
  end;

  ops

let view model =
  Term.render (view_ops model)
