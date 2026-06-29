type emphasis = { italic : bool; bold : bool; strike : bool }

let no_emphasis = { italic = false; bold = false; strike = false }

type role =
  | Body
  | Heading of int
  | Quote
  | Code_span
  | Link of string
  | Marker

type style = { role : role; emphasis : emphasis }
type span = style * string
type line = span list
type code_block = { info : string; code : string; indent : int }

type block =
  | Lines of line list
  | Code of code_block

module C = Cmarkit

(* Plain (unstyled) span helpers for indentation and inter-word spacing. The
   role of whitespace is irrelevant — it has no glyph — so we use [Body]. *)
let plain text = ({ role = Body; emphasis = no_emphasis }, text)
let spaces n = plain (String.make (max 0 n) ' ')

let width_of_spans measure spans =
  List.fold_left (fun acc (_, t) -> acc + measure t) 0 spans

(* {1 Inline walk} -- flatten an inline tree into styled runs. *)

let link_dest (l : C.Inline.Link.t) : string =
  match C.Inline.Link.reference l with
  | `Inline (ld, _) -> (
      match C.Link_definition.dest ld with Some (d, _) -> d | None -> "")
  | `Ref _ -> ""

(* [base] is the block-level role in force (Body / Heading / Quote); inline
   nodes may override it (Code_span, Link). [emph] accumulates emphasis. *)
let rec inline_runs base emph (inline : C.Inline.t) : span list =
  match inline with
  | C.Inline.Text (s, _) -> [ ({ role = base; emphasis = emph }, s) ]
  | C.Inline.Emphasis (e, _) ->
      inline_runs base { emph with italic = true } (C.Inline.Emphasis.inline e)
  | C.Inline.Strong_emphasis (e, _) ->
      inline_runs base { emph with bold = true } (C.Inline.Emphasis.inline e)
  | C.Inline.Code_span (cs, _) ->
      [ ({ role = Code_span; emphasis = emph }, C.Inline.Code_span.code cs) ]
  | C.Inline.Link (l, _) ->
      inline_runs (Link (link_dest l)) emph (C.Inline.Link.text l)
  | C.Inline.Image (l, _) ->
      (* No inline image rendering here; show the alt text as a link. *)
      inline_runs (Link (link_dest l)) emph (C.Inline.Link.text l)
  | C.Inline.Autolink (a, _) ->
      let s, _ = C.Inline.Autolink.link a in
      [ ({ role = Link s; emphasis = emph }, s) ]
  | C.Inline.Break (_, _) -> [ ({ role = base; emphasis = emph }, " ") ]
  | C.Inline.Inlines (is, _) -> List.concat_map (inline_runs base emph) is
  | C.Inline.Ext_strikethrough (st, _) ->
      inline_runs base
        { emph with strike = true }
        (C.Inline.Strikethrough.inline st)
  | _ -> []
(* [Cmarkit.Inline.t] is an EXTENSIBLE variant ([type t = ..]), so a wildcard
   is mandatory -- the compiler cannot prove exhaustiveness. It drops Raw_html,
   Ext_math_span, and any future inline nodes we don't render. *)

(* {1 Word wrapping} *)

(* A word is a run of styled pieces with no internal spaces, plus its display
   width. Words are separated by single spaces when laid out. *)
type word = { pieces : span list; width : int }

(* Tokenize styled runs into words. Spaces separate words; adjacent runs with
   no space between them (e.g. "**bo**ld") merge into a single word so styling
   never forces a break mid-word. *)
let words_of_runs measure (runs : span list) : word list =
  let words = ref [] in
  let cur = ref [] in (* reversed pieces of the word under construction *)
  let cur_w = ref 0 in
  let flush () =
    (match !cur with
    | [] -> ()
    | pieces_rev -> words := { pieces = List.rev pieces_rev; width = !cur_w } :: !words);
    cur := [];
    cur_w := 0
  in
  List.iter
    (fun (style, text) ->
      let parts = String.split_on_char ' ' text in
      List.iteri
        (fun i part ->
          if i > 0 then flush () (* a space preceded this part *);
          if part <> "" then begin
            cur := (style, part) :: !cur;
            cur_w := !cur_w + measure part
          end)
        parts)
    runs;
  flush ();
  List.rev !words

(* Greedily wrap [words] to [width] columns. [first_prefix] leads the first
   line, [cont_prefix] every continuation line (the hanging indent). A word
   wider than the available width is placed alone and allowed to overflow. *)
let wrap_words measure ~width ~first_prefix ~cont_prefix (words : word list) :
    line list =
  let first_avail = width - width_of_spans measure first_prefix in
  let cont_avail = width - width_of_spans measure cont_prefix in
  let lines = ref [] in
  let cur = ref [] in (* reversed text pieces of the current line (no prefix) *)
  let cur_w = ref 0 in
  let line_idx = ref 0 in
  let avail () = if !line_idx = 0 then first_avail else cont_avail in
  let prefix () = if !line_idx = 0 then first_prefix else cont_prefix in
  let flush () =
    lines := (prefix () @ List.rev !cur) :: !lines;
    incr line_idx;
    cur := [];
    cur_w := 0
  in
  List.iter
    (fun w ->
      let started = !cur <> [] in
      let sep = if started then 1 else 0 in
      if started && !cur_w + sep + w.width > avail () then flush ();
      if !cur <> [] then begin
        cur := plain " " :: !cur;
        cur_w := !cur_w + 1
      end;
      List.iter (fun p -> cur := p :: !cur) w.pieces;
      cur_w := !cur_w + w.width)
    words;
  if !cur <> [] || !line_idx = 0 then flush ();
  List.rev !lines

(* {1 Block walk} *)

(* Coalesce adjacent [Lines] blocks into one, keeping [Code] blocks separate.
   Used to render list items tight (no blank line between them). *)
let merge_lines blocks =
  List.fold_right
    (fun blk acc ->
      match (blk, acc) with
      | Lines a, Lines b :: rest -> Lines (a @ b) :: rest
      | _ -> blk :: acc)
    blocks []

(* {1 Tables} *)

(* Drop trailing whitespace from a line (last column padding would otherwise
   leave invisible trailing spaces on every table row). *)
let rstrip_line (line : line) : line =
  let rec drop = function
    | (_, t) :: rest when String.trim t = "" -> drop rest
    | rest -> rest
  in
  match drop (List.rev line) with
  | [] -> []
  | (style, t) :: rest_rev ->
      let n = ref (String.length t) in
      while !n > 0 && t.[!n - 1] = ' ' do decr n done;
      List.rev ((style, String.sub t 0 !n) :: rest_rev)

(* Pad a wrapped cell line to [w] columns under [align]. *)
let align_cell measure ~w ~align (line : span list) : span list =
  let pad = max 0 (w - width_of_spans measure line) in
  if pad = 0 then line
  else
    match align with
    | `Left -> line @ [ spaces pad ]
    | `Right -> spaces pad :: line
    | `Center ->
        let l = pad / 2 in
        [ spaces l ] @ line @ [ spaces (pad - l) ]

(* Render a table to aligned lines, R data.frame style: a bold header row, a
   dashed rule, then data rows. Columns are sized to their widest cell; if the
   row exceeds the available width, the widest columns are capped (water-fill)
   and their cells wrapped, so narrow columns (e.g. numbers) stay intact. *)
let render_table measure ~width ~margin (tbl : C.Block.Table.t) : block list =
  let n = C.Block.Table.col_count tbl in
  if n = 0 then []
  else begin
    (* per-column alignment, taken from the separator row *)
    let aligns = Array.make n `Left in
    (* rows in document order, as (is_header, runs-per-column) *)
    let rows = ref [] in
    let cells_to_runs ~bold cells =
      let arr = Array.make n [] in
      List.iteri
        (fun i (inl, _) ->
          if i < n then
            arr.(i) <-
              inline_runs Body { no_emphasis with bold } inl)
        cells;
      arr
    in
    List.iter
      (fun ((row, _), _) ->
        match row with
        | `Sep seps ->
            List.iteri
              (fun i ((a, _), _) ->
                if i < n then
                  aligns.(i) <- (match a with Some a -> a | None -> `Left))
              seps
        | `Header cells -> rows := (true, cells_to_runs ~bold:true cells) :: !rows
        | `Data cells -> rows := (false, cells_to_runs ~bold:false cells) :: !rows)
      (C.Block.Table.rows tbl);
    let rows = List.rev !rows in
    (* natural (widest-cell) width per column *)
    let natural = Array.make n 0 in
    List.iter
      (fun (_, arr) ->
        Array.iteri
          (fun j runs -> natural.(j) <- max natural.(j) (width_of_spans measure runs))
          arr)
      rows;
    let gap = 2 in
    let avail = max 1 (width - width_of_spans measure margin) in
    let avail_cols = max 1 (avail - (gap * (n - 1))) in
    let max_natural = Array.fold_left max 0 natural in
    (* largest cap C such that sum(min natural C) fits the column budget *)
    let cap =
      if Array.fold_left ( + ) 0 natural <= avail_cols then max_natural
      else
        let rec find c =
          if c <= 1 then 1
          else if Array.fold_left (fun a nj -> a + min nj c) 0 natural <= avail_cols
          then c
          else find (c - 1)
        in
        find max_natural
    in
    let w = Array.map (fun nj -> max 1 (min nj cap)) natural in
    let gap_span = spaces gap in
    (* assemble one row's per-column runs into visual lines (wrapping cells) *)
    let assemble arr : line list =
      let wrapped =
        Array.mapi
          (fun j runs ->
            wrap_words measure ~width:w.(j) ~first_prefix:[] ~cont_prefix:[]
              (words_of_runs measure runs))
          arr
      in
      let height = Array.fold_left (fun a ls -> max a (List.length ls)) 1 wrapped in
      let rec join_cols = function
        | [] -> []
        | [ c ] -> c
        | c :: rest -> c @ [ gap_span ] @ join_cols rest
      in
      List.init height (fun k ->
          let cols =
            Array.to_list
              (Array.mapi
                 (fun j ls ->
                   let cell = match List.nth_opt ls k with Some l -> l | None -> [] in
                   align_cell measure ~w:w.(j) ~align:aligns.(j) cell)
                 wrapped)
          in
          rstrip_line (margin @ join_cols cols))
    in
    let header_rows, data_rows = List.partition (fun (h, _) -> h) rows in
    let rule =
      let rec join_cols = function
        | [] -> []
        | [ c ] -> [ c ]
        | c :: rest -> c :: gap_span :: join_cols rest
      in
      margin
      @ join_cols
          (Array.to_list
             (Array.map
                (fun wj -> ({ role = Marker; emphasis = no_emphasis }, String.make wj '-'))
                w))
    in
    let header_lines = List.concat_map (fun (_, arr) -> assemble arr) header_rows in
    let data_lines = List.concat_map (fun (_, arr) -> assemble arr) data_rows in
    [ Lines (header_lines @ [ rule ] @ data_lines) ]
  end

(* [margin] is the span prefix prepended to every line of this block (the
   accumulated indentation and quote bars from enclosing blocks). *)
let rec render_block measure ~width ~margin (b : C.Block.t) : block list =
  match b with
  | C.Block.Blocks (bs, _) ->
      List.concat_map (render_block measure ~width ~margin) bs
  | C.Block.Paragraph (p, _) ->
      let runs = inline_runs Body no_emphasis (C.Block.Paragraph.inline p) in
      [ Lines (wrap_words measure ~width ~first_prefix:margin ~cont_prefix:margin
                 (words_of_runs measure runs)) ]
  | C.Block.Heading (h, _) ->
      let level = C.Block.Heading.level h in
      let runs =
        inline_runs (Heading level) no_emphasis (C.Block.Heading.inline h)
      in
      [ Lines (wrap_words measure ~width ~first_prefix:margin ~cont_prefix:margin
                 (words_of_runs measure runs)) ]
  | C.Block.Block_quote (bq, _) ->
      let bar = ({ role = Marker; emphasis = no_emphasis }, "\xe2\x94\x82 ") in
      let margin' = margin @ [ bar ] in
      render_block measure ~width ~margin:margin' (C.Block.Block_quote.block bq)
  | C.Block.Code_block (cb, _) ->
      let info =
        match C.Block.Code_block.info_string cb with
        | Some (s, _) -> String.trim s
        | None -> ""
      in
      let code =
        C.Block.Code_block.code cb
        |> List.map C.Block_line.to_string
        |> String.concat "\n"
      in
      [ Code { info; code; indent = width_of_spans measure margin } ]
  | C.Block.List (l, _) -> render_list measure ~width ~margin l
  | C.Block.Ext_table (tbl, _) -> render_table measure ~width ~margin tbl
  | C.Block.Thematic_break _ ->
      let avail = max 0 (width - width_of_spans measure margin) in
      [ Lines
          [ margin
            @ [ ({ role = Marker; emphasis = no_emphasis },
                 String.make (min avail 40) '-') ] ] ]
  | _ -> []
(* [Cmarkit.Block.t] is also an EXTENSIBLE variant, so a wildcard is mandatory.
   It drops Html_block, Blank_line, Link_reference_definition, and the
   extension blocks (tables, footnotes, math) we don't render in v1. *)

and render_list measure ~width ~margin (l : C.Block.List'.t) : block list =
  let items = C.Block.List'.items l in
  let start =
    match C.Block.List'.type' l with
    | `Ordered (n, _) -> Some n
    | `Unordered _ -> None
  in
  let item_blocks =
    List.concat
      (List.mapi
         (fun i (item, _) ->
           let marker =
             match start with
             | Some n -> Printf.sprintf "%d. " (n + i)
             | None -> "\xe2\x80\xa2 " (* bullet "• " *)
           in
           render_list_item measure ~width ~margin ~marker item)
         items)
  in
  merge_lines item_blocks

(* Render a list item's contents indented by the marker width, then overlay the
   marker glyph onto the first text line (replacing that line's indent placeholder). *)
and render_list_item measure ~width ~margin ~marker (item : C.Block.List_item.t)
    : block list =
  let indent = spaces (measure marker) in
  let margin' = margin @ [ indent ] in
  let blocks =
    render_block measure ~width ~margin:margin' (C.Block.List_item.block item)
  in
  let marker_span = ({ role = Marker; emphasis = no_emphasis }, marker) in
  overlay_marker blocks ~at:(List.length margin) ~marker_span

(* Replace the span at index [at] of the first line of the first [Lines] block
   with [marker_span]. That index is the item's indent placeholder. *)
and overlay_marker blocks ~at ~marker_span =
  let done_ = ref false in
  List.map
    (fun blk ->
      if !done_ then blk
      else
        match blk with
        | Code _ -> blk
        | Lines [] -> blk
        | Lines (first :: rest) ->
            done_ := true;
            let first' =
              List.mapi (fun i sp -> if i = at then marker_span else sp) first
            in
            Lines (first' :: rest))
    blocks

let render ~width ~measure md =
  let doc = C.Doc.of_string ~strict:false md in
  render_block measure ~width ~margin:[] (C.Doc.block doc)
