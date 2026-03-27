let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let skip_spaces s i =
  let len = String.length s in
  let rec loop j =
    if j < len && is_space s.[j] then loop (j + 1) else j
  in
  loop i

let read_quoted_string s i =
  let len = String.length s in
  if i >= len then None
  else
    let quote = s.[i] in
    if quote <> '"' && quote <> '\'' then None
    else
      let buf = Buffer.create 32 in
      let rec loop j =
        if j >= len then None
        else
          match s.[j] with
          | c when c = quote -> Some (Buffer.contents buf, j + 1)
          | '\\' when j + 1 < len ->
              Buffer.add_char buf s.[j + 1];
              loop (j + 2)
          | c ->
              Buffer.add_char buf c;
              loop (j + 1)
      in
      loop (i + 1)

let read_theme_name path =
  if not (Sys.file_exists path) then None
  else
    let contents = In_channel.with_open_text path In_channel.input_all in
    let needle = "raoui.theme" in
    let len = String.length contents in
    let needle_len = String.length needle in
    let rec find_from i =
      if i + needle_len > len then None
      else if String.sub contents i needle_len = needle then
        let j = skip_spaces contents (i + needle_len) in
        if j < len && contents.[j] = '=' then
          let k = skip_spaces contents (j + 1) in
          match read_quoted_string contents k with
          | Some (value, _) -> Some value
          | None -> find_from (i + needle_len)
        else
          find_from (i + needle_len)
      else
        find_from (i + 1)
    in
    find_from 0
