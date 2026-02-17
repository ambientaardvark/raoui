open Frontend_types

module Term = Terminal_ops.Make(struct let term_type = "ansi" end)

let completion_max_width = 20
let readline_prompt_max_length = 20
let input_prompt = "input> "

let log message =
  let oc = Stdlib.open_out_gen [Open_append; Open_creat] 0o666 "/Users/alanlee/Documents/Programs/raoui/debug_log.txt" in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.Printf.fprintf oc "%s\n" message)

let prompt_width_for_mode mode =
  match mode with
  | Frontend_types.Readline rl_prompt ->
      if String.length rl_prompt <= readline_prompt_max_length
      then String.length (rl_prompt ^ "> ")
      else String.length input_prompt
  | Frontend_types.Normal -> String.length prompt

let absolute_cursor_pos model =
  let prompt_width = prompt_width_for_mode model.mode in
  let cursor_abs_row = model.prompt_top_row + model.cursor_row in
  let cursor_abs_col = prompt_width + model.cursor_col + 1 in
  (cursor_abs_row, cursor_abs_col)

let print_completion_line line ~selected ~max_width =
  let line =
    if max_width <= 0 then ""
    else if String.length line <= max_width then line
    else String.sub line 0 max_width
  in
  let line =
    let pad_len = max 0 (max_width - String.length line) in
    line ^ String.make pad_len ' '
  in
  let style = if selected then `Completion_selected else `Completion in
  Terminal_ops.Print [(style, line)]

let completion_col_offset model =
  let _, col = absolute_cursor_pos model in
  col

let is_word_char s =
  if String.length s > 1 then true
  else
    let c = String.get s 0 in
    Char.Ascii.is_alphanum c

let should_show_completions model =
  if model.cursor_pos < 2 then false
  else
    match List.nth_opt model.lines model.cursor_line with
    | None -> false
    | Some line ->
      if Unicode_string.length line < model.cursor_pos then false
      else
        let prev1 = Unicode_string.cluster_at line (model.cursor_pos - 1) in
        let prev2 = Unicode_string.cluster_at line (model.cursor_pos - 2) in
        is_word_char prev1 && is_word_char prev2

let view_completions model ops =
  let add op = Queue.add op ops in
  if not (should_show_completions model) then ()
  else
  match model.completion with
  | None -> ()
  | Some cs ->
    let filtered = Completion.filtered_items cs in
    if filtered = [] then ()
    else
      let cursor_row, _ = absolute_cursor_pos model in
      let start_row = cursor_row + 1 in
      let col_offset = completion_col_offset model in
      let terminal_remaining = max 0 (model.term_width - col_offset + 1) in
      let max_width = min completion_max_width terminal_remaining in
      let selected = Completion.selected_index cs in
      let rec loop row idx = function
        | [] -> ()
        | _ when row > model.term_height -> ()
        | _ when idx >= 5 -> ()
        | item :: rest ->
          add (Terminal_ops.Cursor_to (row, col_offset));
          add Terminal_ops.Clear_to_eol;
          add (print_completion_line item ~selected:(idx = selected) ~max_width);
          loop (row + 1) (idx + 1) rest
      in
      loop start_row 0 filtered

let view_ops model =
  let open Terminal_ops in
  let ops = Queue.create () in
  let add op = Queue.add op ops in

  let prompt_width = prompt_width_for_mode model.mode in
  let width = effective_width model in

  (* Convert cached tokens to spans *)
  let highlighted_lines =
    List.map (fun (entry : Syntax.Cache.entry) -> Syntax.tokens_to_spans entry.tokens) model.lex_cache
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

  let show_cursor = match model.mode with
    | Frontend_types.Readline _ -> true  (* Always show cursor in readline *)
    | _ -> not model.awaiting_response
  in
  add (if show_cursor then Show_cursor else Hide_cursor);

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
      let p = match model.mode, i, model.awaiting_response with
        | Frontend_types.Readline rl_prompt, 0, _ ->
            if String.length rl_prompt <= readline_prompt_max_length
            then rl_prompt ^ "> "
            else input_prompt
        | _, 0, false -> prompt
        | _, 0, true -> pending_prompt
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

  let total_box_rows = visible_rows in
  let old_visible_rows = min model.prompt_box_height model.term_height in
  let extra_lines = old_visible_rows - total_box_rows in
  let cursor_after_render = viewport_start + total_box_rows - 1 in
  let possible_space = max 0 (model.term_height - cursor_after_render) in
  let rows_to_clear = max 0 (min extra_lines possible_space) in
  for i = 1 to rows_to_clear do
    let row = cursor_after_render + i in
    if row <= model.term_height then begin
      add (Cursor_to (row, 1));
      add Clear_to_eol
    end
  done;

  (* Render completion dropdown as an overlay after base prompt cleanup. *)
  view_completions model ops;

  (* Position cursor: in output area during eval, in prompt otherwise.
     Exception: during readline mode, always position in prompt area. *)
  (match model.mode, model.awaiting_response with
   | Frontend_types.Readline _, _ ->
       (* Readline mode: cursor in input area even if awaiting *)
       let cursor_abs_row = model.prompt_top_row + model.cursor_row in
       let cursor_abs_col = prompt_width + model.cursor_col + 1 in
       add (Cursor_to (cursor_abs_row, cursor_abs_col))
   | Frontend_types.Normal, true ->
       (* Awaiting response: cursor in output area *)
       let row, col = model.repl_cursor in
       add (Cursor_to (row, col))
   | Frontend_types.Normal, false ->
       (* Normal mode: cursor in input area *)
       let cursor_abs_row = model.prompt_top_row + model.cursor_row in
       let cursor_abs_col = prompt_width + model.cursor_col + 1 in
       add (Cursor_to (cursor_abs_row, cursor_abs_col)));

  ops

let view model =
  Term.render (view_ops model)
