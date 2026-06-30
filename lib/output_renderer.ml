open Frontend_types
module Term = Terminal_ops.Ansi

let percent_encode_path path =
  let is_unreserved = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' | '/' -> true
    | _ -> false
  in
  let buf = Buffer.create (String.length path + 16) in
  String.iter
    (fun c ->
      if is_unreserved c then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    path;
  Buffer.contents buf

let file_url path = "file://" ^ percent_encode_path path

let supports_file_hyperlinks terminal_capabilities =
  match terminal_capabilities.Terminal_capabilities.image_protocol with
  | Terminal_capabilities.Kitty | Terminal_capabilities.ITerm -> true
  | Terminal_capabilities.No_image -> false

let plot_banner_spans ~terminal_capabilities path =
  if supports_file_hyperlinks terminal_capabilities then
    [
      ( `Raw,
        Printf.sprintf "\x1b]8;;%s\x1b\\[click to open plot]\x1b]8;;\x1b\\\n"
          (file_url path) );
    ]
  else [ (`Accent, "[open plot] "); (`Plain, path); (`Comment, "\n") ]

let image_output_spans ~terminal_capabilities (image : Ffi_backend.image) =
  let basename = Filename.basename image.source_path in
  let dims =
    match (image.width_px, image.height_px) with
    | Some w, Some h -> Printf.sprintf " (%dx%d)" w h
    | _ -> ""
  in
  let mime =
    match image.mime_type with
    | Some mime -> Printf.sprintf " %s" mime
    | None -> ""
  in
  [ (`Accent, "[image] "); (`Plain, basename); (`Comment, dims ^ mime ^ "\n") ]
  @ plot_banner_spans ~terminal_capabilities image.source_path

let content_height model =
  let width = Frontend_types.effective_width model in
  model.input.lines |> Frontend_types.wrap_lines width |> List.length |> max 1

(* Settle the input box once output has been printed and the terminal cursor is
   at [out_row]/[out_col]. The box renders only its own content rows, but it is
   kept in the bottom zone (reserving at least [min_prompt_height], more for a
   tall box) so output scrolls up beneath it rather than letting the prompt
   creep down the screen. Scrolls up immediately to make room, preserving the
   output above. *)
let settle_prompt_after_output model ~out_row ~out_col =
  let content = content_height model in
  let next_prompt_row = if out_col = 1 then out_row else out_row + 1 in
  let natural = max model.layout.prompt_top_row next_prompt_row in
  (* Pin the box so it sits in the bottom zone and always fits on screen. *)
  let reserve = max content Frontend_types.min_prompt_height in
  let pinned = max 2 (min natural (model.layout.term_height - reserve + 1)) in
  let scroll_needed = natural - pinned in
  if scroll_needed > 0 then begin
    print_string
      (Term.scroll_up ~term_height:model.layout.term_height scroll_needed);
    flush stdout
  end;
  {
    model with
    repl =
      {
        model.repl with
        repl_output = None;
        repl_cursor = (out_row - scroll_needed, out_col);
      };
    layout =
      {
        model.layout with
        prompt_top_row = pinned;
        prompt_box_height = content;
        scroll_amount = 0;
      };
  }

(* NOTE: This clears from repl_cursor to end of screen before printing output,
   which erases any prompt the user typed while waiting. View then repaints.
   This may not work correctly with output that uses cursor movement (e.g.
   progress bars with \r) - get_cursor_position won't reflect actual extent. *)
let print_repl_output ~terminal_capabilities ~user_options model =
  match model.repl.repl_output with
  | None -> model
  | Some (Output_text []) ->
      let new_row, new_col = Terminal_session.get_cursor_position () in
      settle_prompt_after_output model ~out_row:new_row ~out_col:new_col
  | Some ((Output_text _ | Output_image _ | Output_markdown _) as output) ->
      let row, col = model.repl.repl_cursor in
      let rendered_image =
        match output with
        | Output_image image ->
            Terminal_image.render ~terminal_capabilities ~config:user_options
              ~term_width:model.layout.term_width ~image
        | Output_text _ | Output_markdown _ -> None
      in
      let spans =
        match output with
        | Output_text spans -> spans
        | Output_markdown md ->
            (* Render at the live width; each wrapped line is terminated with a
               newline so the terminal never re-wraps it. *)
            Md_tty.render ~width:model.layout.term_width model.theme md
            |> List.concat_map (fun line -> line @ [ (`Plain, "\n") ])
        | Output_image image -> (
            match rendered_image with
            | Some rendered ->
                let prefix = if col = 1 then "\n" else "\n\n" in
                (* Raw adds SGR resets around the content, which is harmless for
                   kitty APC sequences but would clobber SGR from other renderers. *)
                [ (`Raw, prefix ^ rendered.Terminal_image.output) ]
                @ plot_banner_spans ~terminal_capabilities image.source_path
            | None -> image_output_spans ~terminal_capabilities image)
      in
      print_string (Term.cursor_to row col);
      print_string Term.clear_to_eos;
      print_string (Term.render_spans model.theme spans);
      flush stdout;
      (match (output, rendered_image) with
      | Output_image image, Some _ when image.preview_path <> image.source_path
        -> (
          try Sys.remove image.preview_path with Sys_error _ -> ())
      | _ -> ());
      let new_row, new_col = Terminal_session.get_cursor_position () in
      (* Streaming output relies on the terminal's own scroll; just follow the
         cursor down (never up) and keep the box sized to its content. *)
      let next_prompt_row = if new_col = 1 then new_row else new_row + 1 in
      {
        model with
        repl =
          {
            model.repl with
            repl_output = None;
            repl_cursor = (new_row, new_col);
          };
        layout =
          {
            model.layout with
            prompt_top_row = max model.layout.prompt_top_row next_prompt_row;
            prompt_box_height = content_height model;
          };
      }
