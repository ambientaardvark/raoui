module Term = Terminal_ops.Ansi

let set_raw_mode () =
  let termio = Unix.tcgetattr Unix.stdin in
  let raw_termio =
    {
      termio with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_isig = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
    }
  in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_termio;
  termio

let restore_mode termio = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

let set_solid_cursor () =
  print_string Term.solid_cursor;
  flush stdout

let enable_bracketed_paste () =
  print_string Term.enable_bracketed_paste;
  flush stdout

let disable_bracketed_paste () =
  print_string Term.disable_bracketed_paste;
  flush stdout

(* Populated during init (before Eio) by get_cursor_position, then drained
   by the Eio event loop via tty_listener's prefetched-byte mechanism. *)
let pending_input = Queue.create ()
let enqueue_pending_char c = Queue.push c pending_input
let enqueue_pending_string s = String.iter enqueue_pending_char s

let is_csi_final_byte c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '~'

let parse_cursor_position_response response =
  try Some (Scanf.sscanf response "\x1b[%d;%dR" (fun row col -> (row, col)))
  with Scanf.Scan_failure _ | End_of_file | Failure _ -> None

let get_cursor_position () =
  print_string Term.cursor_position_request;
  flush stdout;
  let rec read_csi_sequence buf =
    let c = input_char stdin in
    Buffer.add_char buf c;
    if is_csi_final_byte c then Buffer.contents buf else read_csi_sequence buf
  in
  let rec loop () =
    match input_char stdin with
    | '\x1b' -> (
        match input_char stdin with
        | '[' -> (
            let buf = Buffer.create 16 in
            Buffer.add_string buf "\x1b[";
            let response = read_csi_sequence buf in
            match parse_cursor_position_response response with
            | Some pos -> pos
            | None ->
                enqueue_pending_string response;
                loop ())
        | c ->
            enqueue_pending_char '\x1b';
            enqueue_pending_char c;
            loop ())
    | c ->
        enqueue_pending_char c;
        loop ()
  in
  loop ()

let get_term_dimensions () =
  let height =
    Terminal_size.get_rows () |> function
    | Some h -> h
    | None -> failwith "can't get terminal height"
  in
  let width =
    Terminal_size.get_columns () |> function
    | Some w -> w
    | None -> failwith "can't get terminal width"
  in
  (width, height)

let sigwinch_pipe_rd, sigwinch_pipe_wr =
  let rd, wr = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock rd;
  Unix.set_nonblock wr;
  (rd, wr)

let sigwinch_byte = Bytes.of_string "x"

let () =
  Sys.set_signal Sys.sigwinch
    (Sys.Signal_handle
       (fun _ ->
         try ignore (Unix.write sigwinch_pipe_wr sigwinch_byte 0 1)
         with Unix.Unix_error _ -> ()))

let rec await_dim_change ~current_w ~current_h =
  Eio_unix.await_readable sigwinch_pipe_rd;
  let buf = Bytes.create 16 in
  (try
     while Unix.read sigwinch_pipe_rd buf 0 16 > 0 do
       ()
     done
   with Unix.Unix_error (Unix.EAGAIN, _, _) -> ());
  let w, h = get_term_dimensions () in
  if w = current_w && h = current_h then await_dim_change ~current_w ~current_h
  else (w, h)
