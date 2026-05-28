type key =
  | Char of string
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
  | Other of string
  | Unknown of string

let default_escape_timeout_sec = 0.05

let pop_prefetched = function
  | Some queue when not (Queue.is_empty queue) -> Some (Queue.pop queue)
  | Some _ | None -> None

let has_prefetched = function
  | Some queue -> not (Queue.is_empty queue)
  | None -> false

let read_byte ~prefetched stdin =
  match pop_prefetched prefetched with
  | Some c -> Some c
  | None ->
      let buf = Cstruct.create 1 in
      (try
         let n = Eio.Flow.single_read stdin buf in
         if n = 0 then None else Some (Cstruct.get_char buf 0)
       with End_of_file -> None)

let utf8_continuation_bytes leading_byte =
  let b = Char.code leading_byte in
  if b land 0x80 = 0 then 0
  else if b land 0xE0 = 0xC0 then 1
  else if b land 0xF0 = 0xE0 then 2
  else if b land 0xF8 = 0xF0 then 3
  else 0

let read_utf8_char ~prefetched stdin leading_byte =
  let n = utf8_continuation_bytes leading_byte in
  if n = 0 then String.make 1 leading_byte
  else
    let bytes = Bytes.create (n + 1) in
    Bytes.set bytes 0 leading_byte;
    let rec read_rest i =
      if i > n then Bytes.to_string bytes
      else
        match read_byte ~prefetched stdin with
        | None -> Bytes.sub_string bytes 0 i
        | Some c ->
            Bytes.set bytes i c;
            read_rest (i + 1)
    in
    read_rest 1

let read_byte_timeout ~prefetched clock stdin timeout_sec =
  match pop_prefetched prefetched with
  | Some c -> Some c
  | None -> (
      match
        Eio.Time.with_timeout clock timeout_sec (fun () ->
            Ok (read_byte ~prefetched:None stdin))
      with
      | Ok result -> result
      | Error `Timeout -> None)

let read_bracketed_paste ~prefetched stdin =
  let buf = Buffer.create 256 in
  let add_chars chars = List.iter (Buffer.add_char buf) chars in
  let rec loop () =
    match read_byte ~prefetched stdin with
    | None -> Buffer.contents buf
    | Some '\x1b' -> check_end [ '\x1b' ]
    | Some c ->
        Buffer.add_char buf c;
        loop ()
  and check_end acc =
    let expected = [ '\x1b'; '['; '2'; '0'; '1'; '~' ] in
    if acc = expected then Buffer.contents buf
    else if List.length acc >= 6 then (
      add_chars acc;
      loop ())
      else
        match read_byte ~prefetched stdin with
        | None ->
            add_chars acc;
            Buffer.contents buf
      | Some c ->
          let next = acc @ [ c ] in
          if List.nth expected (List.length acc) = c then check_end next
          else (
            add_chars next;
            loop ())
  in
  loop ()

let parse_csi_sequence ~prefetched stdin =
  (* Read all parameter bytes until we hit a final byte (letter or ~) *)
  let rec read_until_final acc =
    match read_byte ~prefetched stdin with
    | None -> (List.rev acc, None)
    | Some c when (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '~' ->
        (List.rev acc, Some c)
    | Some c -> read_until_final (c :: acc)
  in
  let param_chars, final = read_until_final [] in
  let params = String.init (List.length param_chars) (List.nth param_chars) in
  match final with
  | None -> Unknown (Printf.sprintf "\x1b[%s" params)
  | Some 'A' -> Up
  | Some 'B' -> Down
  | Some 'C' -> Right
  | Some 'D' -> Left
  | Some 'H' -> Home
  | Some 'F' -> End
  | Some '~' -> (
      match params with
      | "3" -> Delete
      | "200" -> Paste (read_bracketed_paste ~prefetched stdin)
      | "27;5;13" -> Ctrl '\r' (* Ctrl+Enter *)
      | _ -> Unknown (Printf.sprintf "\x1b[%s~" params))
  | Some c -> Unknown (Printf.sprintf "\x1b[%s%c" params c)

let parse_ss3_sequence ~prefetched ~escape_timeout_sec clock stdin =
  match read_byte_timeout ~prefetched clock stdin escape_timeout_sec with
  | None -> Unknown "\x1bO"
  | Some 'A' -> Up
  | Some 'B' -> Down
  | Some 'C' -> Right
  | Some 'D' -> Left
  | Some 'H' -> Home
  | Some 'F' -> End
  | Some c -> Unknown (Printf.sprintf "\x1bO%c" c)

let parse_escape ~prefetched ~escape_timeout_sec clock stdin =
  match read_byte_timeout ~prefetched clock stdin escape_timeout_sec with
  | None -> Escape
  | Some '[' -> parse_csi_sequence ~prefetched stdin
  | Some 'O' -> parse_ss3_sequence ~prefetched ~escape_timeout_sec clock stdin
  | Some '\n' -> Ctrl '\r' (* newline on linux *)
  | Some 'b' -> Other "last word"
  | Some 'f' -> Other "next word"
  | Some c -> Unknown (Printf.sprintf "\x1b%c" c)

let await_input_with_timeout ~prefetched ~escape_timeout_sec ~clock ~stdin =
  match read_byte ~prefetched stdin with
  | None -> Escape (* EOF, treat as escape? *)
  | Some '\x1b' -> parse_escape ~prefetched ~escape_timeout_sec clock stdin
  | Some '\x7f' -> Backspace
  | Some '\t' -> Tab
  | Some '\r' | Some '\n' -> Enter
  | Some c when Char.code c < 32 -> Ctrl (Char.chr (Char.code c + 96))
  | Some c -> Char (read_utf8_char ~prefetched stdin c)

let await_input ~prefetched ~clock ~stdin =
  await_input_with_timeout ~prefetched
    ~escape_timeout_sec:default_escape_timeout_sec ~clock ~stdin

let drain_to_keys_with_timeouts ~prefetched ~escape_timeout_sec
    ~settle_timeout_sec ~clock ~stdin =
  let rec loop acc =
    if has_prefetched prefetched then
        let key =
          await_input_with_timeout ~prefetched ~escape_timeout_sec ~clock ~stdin
        in
        loop (key :: acc)
    else
        let timeout = if acc = [] then 0.0 else settle_timeout_sec in
        let ready, _, _ = Unix.select [ Unix.stdin ] [] [] timeout in
        if ready = [] then List.rev acc
        else
          let key =
            await_input_with_timeout ~prefetched ~escape_timeout_sec ~clock ~stdin
          in
          loop (key :: acc)
  in
  loop []
