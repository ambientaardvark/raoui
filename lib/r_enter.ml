let continuation_indent_size = 2

let leading_spaces s =
  let rec loop i =
    if i >= String.length s then i
    else if String.get s i = ' ' then loop (i + 1)
    else i
  in
  loop 0

type action =
  | Submit
  | Insert_newline of { indent : int }
  | Expand_braces of {
      inner_indent : int;
      outer_indent : int;
    }

let action ~lines ~cursor_line ~cursor_pos ~cache =
  let line = List.nth lines cursor_line in
  let line_text = Unicode_string.to_string line in
  let base_indent = leading_spaces line_text in
  if String.trim line_text = "" then Submit
  else
    match R_lex_cache.get_line_tokens cache ~line:cursor_line with
    | Some line_tokens ->
        let cursor_byte =
          Lexer_cache.cursor_byte_offset ~line ~cursor_pos
        in
        if
          R_continuation.inside_empty_brackets ~tokens:line_tokens
            ~cursor_byte_offset:cursor_byte
        then
          Expand_braces
            {
              inner_indent = base_indent + continuation_indent_size;
              outer_indent = base_indent;
            }
        else (
          let tokens =
            R_lex_cache.tokens_before_line cache ~line:(cursor_line + 1)
          in
          match R_continuation.analyze tokens with
          | R_continuation.Submit -> Submit
          | R_continuation.Continue { indent_levels; in_empty_brackets = _ } ->
              let indent =
                max base_indent (indent_levels * continuation_indent_size)
              in
              Insert_newline { indent })
    | None ->
        (* cursor_line should always index a valid line, so this is unreachable *)
        Submit
