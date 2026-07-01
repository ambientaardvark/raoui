open Frontend_types

module Make (Term : Terminal_ops.TERMINAL) = struct
  let completion_max_width = 30
  let readline_prompt_max_length = 20
  let input_prompt = "input: "
  let shell_prompt = "shell> "
  let ai_prompt = "ai> "
  let search_prompt_prefix = "r-search: "
  let search_prompt_suffix = "| >"

  let rendered_readline_prompt rl_prompt =
    if rl_prompt = "" || rl_prompt = "input" then input_prompt
    else if String.length rl_prompt <= readline_prompt_max_length then rl_prompt
    else input_prompt

  let prompt_width_for_mode mode =
    match mode with
    | Readline rl_prompt -> String.length (rendered_readline_prompt rl_prompt)
    | Shell -> String.length shell_prompt
    | Ai -> String.length ai_prompt
    | Normal -> String.length prompt
    | History_search s ->
        String.length search_prompt_prefix
        + Unicode_string.display_width s
        + String.length search_prompt_suffix

  let absolute_cursor_pos model ~cursor_row ~cursor_col =
    let prompt_width = prompt_width_for_mode model.input.mode in
    let cursor_abs_row = model.layout.prompt_top_row + cursor_row in
    let cursor_abs_col = prompt_width + cursor_col + 1 in
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

  let awaiting_first_output_chunk model =
    let out_row, out_col = model.repl.repl_cursor in
    model.repl.awaiting_response && out_col = 1
    && model.layout.prompt_top_row = out_row + 1

  let should_show_completions model =
    model.input.mode = Frontend_types.Normal
    &&
    match model.input.completion with
    | None -> false
    | Some cs -> Completion.filtered_items cs <> []

  type styled_segment = {
    style : Terminal_ops.style;
    start_byte : int;
    end_byte : int;
  }

  let add_segment row style start_byte end_byte =
    if start_byte = end_byte then row
    else
      match row with
      | prev :: rest when prev.style = style && prev.end_byte = start_byte ->
          { prev with end_byte } :: rest
      | _ -> { style; start_byte; end_byte } :: row

  let rec advance_ranges byte = function
    | range :: rest when range.R_highlight.end_byte <= byte ->
        advance_ranges byte rest
    | ranges -> ranges

  let style_at byte ranges =
    match ranges with
    | range :: _
      when byte >= range.R_highlight.start_byte
           && byte < range.R_highlight.end_byte ->
        range.R_highlight.style
    | _ -> `Plain

  let ranges_to_segments ranges =
    List.map
      (fun (range : R_highlight.styled_range) ->
        {
          style = range.style;
          start_byte = range.start_byte;
          end_byte = range.end_byte;
        })
      ranges

  let wrap_styled_line width line ranges =
    if width <= 0 then [ ranges_to_segments ranges ]
    else if Unicode_string.is_empty line then [ [] ]
    else
      let len = Unicode_string.length line in
      let total_width = Unicode_string.display_width line in
      let rows_rev = ref [] in
      let row_rev = ref [] in
      let row_width = ref 0 in
      let active_ranges = ref ranges in
      let flush_row () =
        rows_rev := List.rev !row_rev :: !rows_rev;
        row_rev := [];
        row_width := 0
      in
      for i = 0 to len - 1 do
        let cluster_width = Unicode_string.width_at line i in
        if !row_width = width then flush_row ();
        if !row_width > 0 && !row_width + cluster_width > width then
          flush_row ();
        let start_byte, end_byte = Unicode_string.byte_range_at line i in
        active_ranges := advance_ranges start_byte !active_ranges;
        let style = style_at start_byte !active_ranges in
        row_rev := add_segment !row_rev style start_byte end_byte;
        row_width := !row_width + cluster_width;
        if !row_width > width then flush_row ()
      done;
      if !row_rev <> [] then flush_row ();
      let rows = List.rev !rows_rev in
      if total_width > 0 && total_width mod width = 0 then rows @ [ [] ]
      else rows

  let spans_of_segments line segments =
    List.map
      (fun { style; start_byte; end_byte } ->
        (style, Unicode_string.slice_bytes line ~start_byte ~end_byte))
      segments

  let view_completions model ops ~cursor_abs_row ~cursor_abs_col =
    let add op = Queue.add op ops in
    if not (should_show_completions model) then ()
    else
      match model.input.completion with
      | None -> ()
      | Some cs ->
          let visible = Completion.visible_items cs in
          if visible = [] then ()
          else
            let start_row = cursor_abs_row + 1 in
            let col_offset = cursor_abs_col in
            let terminal_remaining =
              max 0 (model.layout.term_width - col_offset + 1)
            in
            let max_width = min completion_max_width terminal_remaining in
            let selected = Completion.selected_index_in_window cs in
            let rec loop row idx = function
              | [] -> ()
              | _ when row > model.layout.term_height -> ()
              | item :: rest ->
                  add (Terminal_ops.Cursor_to (row, col_offset));
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

    let width = effective_width model in
    let cursor_row, cursor_col = cursor_terminal_pos model in
    let cursor_abs_row, cursor_abs_col =
      absolute_cursor_pos model ~cursor_row ~cursor_col
    in

    (* Convert cached tokens to source byte ranges. Wrapping is driven by the
       original Unicode_string.t line so we do not re-segment highlighted
       strings. *)
    let highlighted_ranges =
      List.map
        (fun (entry : R_lex_cache.entry) -> R_highlight.ranges_for_entry entry)
        model.input.lex_cache
    in

    let wrapped_rows =
      List.map2
        (fun line ranges ->
          wrap_styled_line width line ranges
          |> List.map (fun segments -> (line, spans_of_segments line segments)))
        model.input.lines highlighted_ranges
      |> List.concat
    in
    let total_rows = List.length wrapped_rows in

    let show_cursor =
      match model.input.mode with
      | Frontend_types.Readline _ -> true
      | Frontend_types.History_search _ -> false
      | _ -> not model.repl.awaiting_response
    in
    add (if show_cursor then Show_cursor else Hide_cursor);

    if model.layout.scroll_amount < 0 then begin
      add (Cursor_to (model.layout.term_height, 1));
      for _ = 1 to -model.layout.scroll_amount do
        add Newline
      done
    end;

    let viewport_start = max 1 model.layout.prompt_top_row in
    add (Cursor_to (viewport_start, 1));
    let skip_rows = viewport_start - model.layout.prompt_top_row in

    List.iteri
      (fun i (_line, content) ->
        if i >= skip_rows && i < skip_rows + model.layout.term_height then begin
          add Clear_to_eol;
          let p =
            match (model.input.mode, i, model.repl.awaiting_response) with
            | Readline rl_prompt, 0, _ -> rendered_readline_prompt rl_prompt
            | Shell, 0, _ -> shell_prompt
            | Ai, 0, _ -> ai_prompt
            | Readline _, _, _ | Frontend_types.Shell, _, _
            | Frontend_types.Ai, _, _ ->
                assert false
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
          (match model.input.mode with
          | Shell -> add (Print ((`Shell_prompt, p) :: content))
          | Ai -> add (Print ((`Ai_prompt, p) :: content))
          | _ -> add (Print ((`Accent, p) :: content)));
          if i < skip_rows + model.layout.term_height - 1 && i < total_rows - 1
          then add Newline
        end)
      wrapped_rows;

    let visible_rows =
      max 0 (min (total_rows - skip_rows) model.layout.term_height)
    in

    let total_box_rows = visible_rows in
    let cursor_after_render = viewport_start + total_box_rows - 1 in
    let first_row_to_clear = max viewport_start (cursor_after_render + 1) in
    for row = first_row_to_clear to model.layout.term_height do
      add (Cursor_to (row, 1));
      add Clear_to_eol
    done;

    (* Clear the reserved output row before the first chunk arrives so stale
     completion overlay content does not linger there. Once output has been
     printed, clearing this row would erase streamed content mid-response. *)
    if awaiting_first_output_chunk model then begin
      let out_row, _ = model.repl.repl_cursor in
      if out_row >= 1 && out_row <= model.layout.term_height then begin
        add (Cursor_to (out_row, 1));
        add Clear_to_eol
      end
    end;

    (* Render completion dropdown as an overlay after base prompt cleanup. *)
    view_completions model ops ~cursor_abs_row ~cursor_abs_col;

    (* Position cursor: in output area during eval, in prompt otherwise.
     Exception: during readline/search mode, always position in prompt area. *)
    (match (model.input.mode, model.repl.awaiting_response) with
    | Readline _, _ ->
        (* Readline mode: cursor in input area even if awaiting *)
        add (Cursor_to (cursor_abs_row, cursor_abs_col))
    | Shell, _ -> add (Cursor_to (cursor_abs_row, cursor_abs_col))
    | Ai, _ -> add (Cursor_to (cursor_abs_row, cursor_abs_col))
    | History_search _, _ ->
        (* History search: cursor in the search input within the prompt *)
        let cursor_abs_row = model.layout.prompt_top_row in
        let cursor_abs_col =
          String.length search_prompt_prefix + cursor_col + 1
        in
        add (Cursor_to (cursor_abs_row, cursor_abs_col))
    | Normal, true ->
        (* Awaiting response: cursor in output area *)
        let row, col = model.repl.repl_cursor in
        add (Cursor_to (row, col))
    | Normal, false ->
        (* Normal mode: cursor in input area *)
        add (Cursor_to (cursor_abs_row, cursor_abs_col)));

    ops

  let view model = Term.render model.theme (view_ops model)
end
