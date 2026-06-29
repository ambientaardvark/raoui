(* Terminal display width of a raw string; falls back to byte length on invalid
   UTF-8 (which the renderer should never produce). *)
let measure s =
  match Unicode_string.of_string s with
  | Ok t -> Unicode_string.display_width t
  | Error _ -> String.length s

let face_of theme (style : Md_layout.style) : Theme.face =
  let base : Theme.face =
    match style.role with
    | Md_layout.Body -> theme.Theme.plain
    | Md_layout.Heading _ -> theme.Theme.accent
    | Md_layout.Quote -> theme.Theme.comment
    | Md_layout.Code_span -> theme.Theme.string
    | Md_layout.Link _ -> theme.Theme.constant
    | Md_layout.Marker -> theme.Theme.keyword
  in
  let is_heading = match style.role with Md_layout.Heading _ -> true | _ -> false in
  let is_quote = match style.role with Md_layout.Quote -> true | _ -> false in
  let is_link = match style.role with Md_layout.Link _ -> true | _ -> false in
  let e : Md_layout.emphasis = style.emphasis in
  {
    base with
    Theme.bold = base.Theme.bold || e.bold || is_heading;
    Theme.italic = base.Theme.italic || e.italic || is_quote;
    Theme.underline = base.Theme.underline || is_link;
    Theme.strike = base.Theme.strike || e.strike;
  }

let span_of theme (mstyle, text) : Terminal_ops.span =
  (`Face (face_of theme mstyle), text)

let highlight_code theme ~info (code : string) : Terminal_ops.span list list =
  ignore theme;
  let lang = String.lowercase_ascii (String.trim info) in
  let lines = String.split_on_char '\n' code in
  match lang with
  | "r" | "" ->
      (* Untagged and R fences both go through R highlighting -- in an R REPL,
         unlabelled code from the assistant is overwhelmingly R. Mode threads
         across lines for multi-line strings. *)
      let _, rev =
        List.fold_left
          (fun (mode, acc) line ->
            let spans, mode' = R_highlight.highlight_line mode line in
            (mode', spans :: acc))
          (R_lexer.Normal, []) lines
      in
      List.rev rev
  | _ -> List.map (fun line -> [ (`Plain, line) ]) lines

let render ~width theme md : Terminal_ops.span list list =
  let blocks = Md_layout.render ~width ~measure md in
  let lines_of_block = function
    | Md_layout.Lines ls -> List.map (List.map (span_of theme)) ls
    | Md_layout.Code { info; code; indent } ->
        let hl = highlight_code theme ~info code in
        if indent <= 0 then hl
        else
          let pad = (`Plain, String.make indent ' ') in
          List.map (fun line -> pad :: line) hl
  in
  (* Separate blocks with a single blank line. *)
  let rec join = function
    | [] -> []
    | [ x ] -> x
    | x :: rest -> x @ [ [] ] @ join rest
  in
  join (List.map lines_of_block blocks)
