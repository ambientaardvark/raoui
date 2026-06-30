(** Renders markdown into raoui terminal spans.

    The only module that knows both [Md_layout] and
    [Terminal_ops]/[Theme]/[R_highlight]. All markdown-specific layout lives in
    the pure [Md_layout] core; this adapter just resolves styles to faces,
    highlights fenced code, and joins blocks into terminal lines. *)

(* Resolve a markdown style to a concrete terminal face. This is where raoui's
   prose styling is chosen (heading colour, link underline, quote italics),
   with inline emphasis stacked on top. *)
val face_of : Theme.t -> Md_layout.style -> Theme.face

(* Highlight a fenced code block into terminal spans, one span list per code
   line. "r"/"R" (and untagged) blocks go through R_highlight; other languages
   render flat. *)
val highlight_code :
  Theme.t -> info:string -> string -> Terminal_ops.span list list

(* Render a COMPLETE markdown document to terminal lines, ready for
   [render_spans]. [width] is the live terminal width at display time; blocks
   are separated by a blank line. *)
val render : width:int -> Theme.t -> string -> Terminal_ops.span list list
