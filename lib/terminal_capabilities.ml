type image_protocol =
  | Kitty
  | ITerm
  | No_image

type t = {
  image_protocol : image_protocol;
}

let detect ?(getenv = Sys.getenv_opt) () =
  let nonempty name =
    match getenv name with
    | Some "" | None -> None
    | Some value -> Some value
  in
  let contains haystack needle =
    let haystack = String.lowercase_ascii haystack in
    let needle = String.lowercase_ascii needle in
    let haystack_len = String.length haystack in
    let needle_len = String.length needle in
    let rec loop i =
      if i + needle_len > haystack_len then false
      else if String.sub haystack i needle_len = needle then true
      else loop (i + 1)
    in
    loop 0
  in
  let image_protocol =
    match nonempty "KITTY_WINDOW_ID" with
    | Some _ -> Kitty
    | None -> (
        match (nonempty "TERM_PROGRAM", nonempty "TERM") with
        | Some term_program, _
          when contains term_program "ghostty" ->
            Kitty
        | Some "iTerm.app", _
        | Some "iTerm2", _ -> ITerm
        | _, Some term when contains term "ghostty" -> Kitty
        | _, Some "iterm2" -> ITerm
        | _ -> No_image)
  in
  { image_protocol }

let supports_inline_images t =
  match t.image_protocol with
  | Kitty | ITerm -> true
  | No_image -> false
