type key =
  | Char of char
  | Ctrl of char
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Delete
  | Backspace
  | Tab
  | Enter
  | Escape
  | Paste of string
  | Unknown of string

let escape_timeout_sec = 0.05

let read_byte stdin =
  let buf = Cstruct.create 1 in
  let n = Eio.Flow.single_read stdin buf in
  if n = 0 then None else Some (Cstruct.get_char buf 0)

let read_byte_timeout clock stdin timeout_sec =
  match
    Eio.Time.with_timeout clock timeout_sec (fun () -> Ok (read_byte stdin))
  with
  | Ok result -> result
  | Error `Timeout -> None

let read_until_tilde stdin =
  let rec loop acc =
    match read_byte stdin with
    | None -> List.rev acc
    | Some '~' -> List.rev acc
    | Some c -> loop (c :: acc)
  in
  loop []

let read_bracketed_paste stdin =
  let buf = Buffer.create 256 in
  let rec loop () =
    match read_byte stdin with
    | None -> Buffer.contents buf
    | Some '\x1b' -> (
        (* Check for end sequence \x1b[201~ *)
        match
          ( read_byte stdin,
            read_byte stdin,
            read_byte stdin,
            read_byte stdin,
            read_byte stdin )
        with
        | Some '[', Some '2', Some '0', Some '1', Some '~' ->
            Buffer.contents buf
        | _ ->
            Buffer.add_char buf '\x1b';
            loop ())
    | Some c ->
        Buffer.add_char buf c;
        loop ()
  in
  loop ()

let parse_csi_sequence stdin =
  match read_byte stdin with
  | None -> Unknown "\x1b["
  | Some 'A' -> Up
  | Some 'B' -> Down
  | Some 'C' -> Right
  | Some 'D' -> Left
  | Some 'H' -> Home
  | Some 'F' -> End
  | Some '3' -> (
      match read_byte stdin with Some '~' -> Delete | _ -> Unknown "\x1b[3")
  | Some '2' -> (
      (* Could be \x1b[200~ for bracketed paste *)
      match (read_byte stdin, read_byte stdin, read_byte stdin) with
      | Some '0', Some '0', Some '~' -> Paste (read_bracketed_paste stdin)
      | _ -> Unknown "\x1b[2")
  | Some c ->
      (* Read until ~ for extended sequences like \x1b[1;5C *)
      let rest = read_until_tilde stdin in
      Unknown
        (Printf.sprintf "\x1b[%c%s~" c
           (String.init (List.length rest) (List.nth rest)))

let parse_escape clock stdin =
  match read_byte_timeout clock stdin escape_timeout_sec with
  | None -> Escape
  | Some '[' -> parse_csi_sequence stdin
  | Some c -> Unknown (Printf.sprintf "\x1b%c" c)

let await_input ~clock ~stdin =
  match read_byte stdin with
  | None -> Escape (* EOF, treat as escape? *)
  | Some '\x1b' -> parse_escape clock stdin
  | Some '\x7f' -> Backspace
  | Some '\t' -> Tab
  | Some '\r' | Some '\n' -> Enter
  | Some c when Char.code c < 32 -> Ctrl (Char.chr (Char.code c + 96))
  | Some c -> Char c
