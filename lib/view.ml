open Frontend_types

module Make (Term : Terminal_ops.TERMINAL) = struct
  let completion_max_width = 30
  let readline_prompt_max_length = 20
  let input_prompt = "input> "
  let shell_prompt = "shell> "
  let search_prompt_prefix = "r-search: "
  let search_prompt_suffix = "| >"

  let prompt_width_for_mode mode =
    match mode with
    | Readline rl_prompt ->
        if String.length rl_prompt <= readline_prompt_max_length then
          String.length (rl_prompt ^ "> ")
        else String.length input_prompt
    | Shell -> String.length shell_prompt
    | Normal -> String.length prompt
    | History_search s ->
        String.length search_prompt_prefix
        + Unicode_string.display_width s
        + String.length search_prompt_suffix

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
    Terminal_ops.Print [ (style, line) ]

  let completion_col_offset model =
    let _, col = absolute_cursor_pos model in
    col

  let awaiting_first_output_chunk model =
    let out_row, out_col = model.repl_cursor in
    model.awaiting_response && out_col = 1 && model.prompt_top_row = out_row + 1

  let should_show_completions model =
    model.mode = Frontend_types.Normal
    &&
    match model.completion with
    | None -> false
    | Some cs -> Completion.filtered_items cs <> []

  let push_span acc style text =
    if text = "" then acc
    else
      match acc with
      | (prev_style, prev_text) :: rest when prev_style = style ->
          (style, prev_text ^ text) :: rest
      | _ -> (style, text) :: acc

  let wrap_spans width spans =
    if width <= 0 then [ spans ]
    else
      let prepared =
        List.filter_map
          (fun (style, text) ->
            if text = "" then None
            else
              match Unicode_string.of_string text with
              | Ok us -> Some (style, us)
              | Error _ -> Some (style, Unicode_string.empty))
          spans
      in
      let total_width =
        List.fold_left
          (fun acc (_, us) -> acc + Unicode_string.display_width us)
          0 prepared
      in
      let rows_rev = ref [] in
      let row_rev = ref [] in
      let row_width = ref 0 in
      let flush_row () =
        rows_rev := List.rev !row_rev :: !rows_rev;
        row_rev := [];
        row_width := 0
      in
      let add_piece style us start len =
        if len > 0 then
          let piece =
            Unicode_string.sub us ~start ~len |> Unicode_string.to_string
          in
          row_rev := push_span !row_rev style piece
      in
      let add_wide_piece style us idx =
        add_piece style us idx 1;
        row_width := !row_width + Unicode_string.width_at us idx;
        flush_row ()
      in
      List.iter
        (fun (style, us) ->
          let span_len = Unicode_string.length us in
          let rec consume idx =
            if idx >= span_len then ()
            else if !row_width = width then begin
              flush_row ();
              consume idx
            end
            else
              let available = width - !row_width in
              let rec find_end end_idx used_width =
                if end_idx >= span_len then (end_idx, used_width)
                else
                  let cluster_width = Unicode_string.width_at us end_idx in
                  if used_width + cluster_width > available then
                    (end_idx, used_width)
                  else find_end (end_idx + 1) (used_width + cluster_width)
              in
              let end_idx, used_width = find_end idx 0 in
              if end_idx = idx then
                if !row_width > 0 then begin
                  flush_row ();
                  consume idx
                end
                else begin
                  add_wide_piece style us idx;
                  consume (idx + 1)
                end
              else begin
                add_piece style us idx (end_idx - idx);
                row_width := !row_width + used_width;
                if !row_width = width then flush_row ();
                consume end_idx
              end
          in
          consume 0)
        prepared;
      if !row_rev <> [] || prepared = [] then flush_row ();
      let rows = List.rev !rows_rev in
      if total_width > 0 && total_width mod width = 0 then rows @ [ [] ]
      else rows

  let wrap_line_with_spans width line spans =
    let wrapped_text = wrap_line width line in
    let wrapped_spans = wrap_spans width spans in
    if List.length wrapped_text = List.length wrapped_spans then
      List.combine wrapped_text wrapped_spans
    else
      List.map
        (fun chunk -> (chunk, [ (`Plain, Unicode_string.to_string chunk) ]))
        wrapped_text

  let view_completions model ops =
    let add op = Queue.add op ops in
    if not (should_show_completions model) then ()
    else
      match model.completion with
      | None -> ()
      | Some cs ->
          let visible = Completion.visible_items cs in
          if visible = [] then ()
          else
            let cursor_row, _ = absolute_cursor_pos model in
            let start_row = cursor_row + 1 in
            let col_offset = completion_col_offset model in
            let terminal_remaining =
              max 0 (model.term_width - col_offset + 1)
            in
            let max_width = min completion_max_width terminal_remaining in
            let selected = Completion.selected_index_in_window cs in
            let rec loop row idx = function
              | [] -> ()
              | _ when row > model.term_height -> ()
              | item :: rest ->
                  add (Terminal_ops.Cursor_to (row, col_offset));
                  add Terminal_ops.Clear_to_eol;
                  add
                    (print_completion_line (Completion.label item)
                       ~selected:(selected = Some idx) ~max_width);
                  loop (row + 1) (idx + 1) rest
            in
            loop start_row 0 visible

  let view_ops model =
    let open Terminal_ops in
    let ops = Queue.create () in
    let add op = Queue.add op ops in

    let prompt_width = prompt_width_for_mode model.mode in
    let width = effective_width model in

    (* Convert cached tokens to spans *)
    let highlighted_lines =
      List.map
        (fun (entry : R_lex_cache.entry) -> R_highlight.render_entry entry)
        model.lex_cache
    in

    (* Wrap each line's spans for display *)
    let wrapped_rows =
      List.map2 (wrap_line_with_spans width) model.lines highlighted_lines
      |> List.concat
    in
    let total_rows = List.length wrapped_rows in

    let show_cursor =
      match model.mode with
      | Frontend_types.Readline _ -> true
      | Frontend_types.History_search _ -> false
      | _ -> not model.awaiting_response
    in
    add (if show_cursor then Show_cursor else Hide_cursor);

    if model.scroll_amount < 0 then begin
      add (Cursor_to (model.term_height, 1));
      for _ = 1 to -model.scroll_amount do
        add Newline
      done
    end;

    let viewport_start = max 1 model.prompt_top_row in
    add (Cursor_to (viewport_start, 1));
    let skip_rows = viewport_start - model.prompt_top_row in

    List.iteri
      (fun i (_line, content) ->
        if i >= skip_rows && i < skip_rows + model.term_height then begin
          add Clear_to_eol;
          let p =
            match (model.mode, i, model.awaiting_response) with
            | Readline rl_prompt, 0, _ ->
                if String.length rl_prompt <= readline_prompt_max_length then
                  rl_prompt ^ "> "
                else input_prompt
            | Shell, 0, _ -> shell_prompt
            | Readline _, _, _ | Frontend_types.Shell, _, _ -> assert false
            | History_search search_input, 0, _ ->
                let search_str = Unicode_string.to_string search_input in
                search_prompt_prefix ^ search_str ^ search_prompt_suffix
            | History_search s, _, _ ->
                let pad_width = prompt_width_for_mode (History_search s) in
                String.make pad_width ' '
            | Normal, 0, false -> prompt
            | Normal, 0, true -> pending_prompt
            | Normal, _, _ -> continued_prompt
          in
          if model.mode = Shell then add (Print ((`Shell_prompt, p) :: content))
          else add (Print ((`Accent, p) :: content));
          if i < skip_rows + model.term_height - 1 && i < total_rows - 1 then
            add Newline
        end)
      wrapped_rows;

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

    (* Clear the reserved output row before the first chunk arrives so stale
     completion overlay content does not linger there. Once output has been
     printed, clearing this row would erase streamed content mid-response. *)
    if awaiting_first_output_chunk model then begin
      let out_row, _ = model.repl_cursor in
      if out_row >= 1 && out_row <= model.term_height then begin
        add (Cursor_to (out_row, 1));
        add Clear_to_eol
      end
    end;

    (* Render completion dropdown as an overlay after base prompt cleanup. *)
    view_completions model ops;

    (* Position cursor: in output area during eval, in prompt otherwise.
     Exception: during readline/search mode, always position in prompt area. *)
    (match (model.mode, model.awaiting_response) with
    | Readline _, _ ->
        (* Readline mode: cursor in input area even if awaiting *)
        let cursor_abs_row = model.prompt_top_row + model.cursor_row in
        let cursor_abs_col = prompt_width + model.cursor_col + 1 in
        add (Cursor_to (cursor_abs_row, cursor_abs_col))
    | Shell, _ ->
        let cursor_abs_row = model.prompt_top_row + model.cursor_row in
        let cursor_abs_col = prompt_width + model.cursor_col + 1 in
        add (Cursor_to (cursor_abs_row, cursor_abs_col))
    | History_search _, _ ->
        (* History search: cursor in the search input within the prompt *)
        let cursor_abs_row = model.prompt_top_row in
        let cursor_abs_col =
          String.length search_prompt_prefix + model.cursor_col + 1
        in
        add (Cursor_to (cursor_abs_row, cursor_abs_col))
    | Normal, true ->
        (* Awaiting response: cursor in output area *)
        let row, col = model.repl_cursor in
        add (Cursor_to (row, col))
    | Normal, false ->
        (* Normal mode: cursor in input area *)
        let cursor_abs_row = model.prompt_top_row + model.cursor_row in
        let cursor_abs_col = prompt_width + model.cursor_col + 1 in
        add (Cursor_to (cursor_abs_row, cursor_abs_col)));

    ops

  let view model = Term.render model.theme (view_ops model)
end
