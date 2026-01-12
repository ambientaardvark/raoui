let csi = "\x1b["
let escape code () = Printf.printf "%s%s%!" csi code
let alt_screen_seq = escape "?1049h"
let exit_alt_screen_seq = escape "?1049l"
let _cursor_up_seq x = escape (Printf.sprintf "%dA" x)
let print_flush s = Printf.printf "%s%!" s


let set_raw_mode () =
  let termio = Unix.tcgetattr Unix.stdin in
  let raw_termio = { termio with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_termio;
  termio


let hello_world () =
  alt_screen_seq ();
  let orig_termio = set_raw_mode () in
  print_flush "Hello, world!";

  let rec wait_for_q () =
    let char_buf = Bytes.create 1 in
    let _ = Unix.read Unix.stdin char_buf 0 1 in
    if char_buf = String.to_bytes "q" then () else wait_for_q ()
  in
  wait_for_q ();
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH orig_termio;
  exit_alt_screen_seq ()


let () = hello_world ()
