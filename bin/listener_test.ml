open Raoui.Tty_listener

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

let restore_mode termio =
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

let examine () =
  let desc = match await_input () with
    | Char key -> Printf.sprintf "Char '%c' (0x%02x)" key (Char.code key)
    | Ctrl key -> Printf.sprintf "Ctrl-%c" (Char.uppercase_ascii key)
    | Up -> "Up"
    | Down -> "Down"
    | Left -> "Left"
    | Right -> "Right"
    | Home -> "Home"
    | End -> "End"
    | Delete -> "Delete"
    | Backspace -> "Backspace"
    | Tab -> "Tab"
    | Enter -> "Enter"
    | Escape -> "Escape"
    | Paste s -> Printf.sprintf "Paste (%d chars): %S" (String.length s) s
    | Unknown s -> Printf.sprintf "Unknown: %S" s
  in
  Printf.printf "%s\n%!" desc

let () =
  Printf.printf "Starting listener test...\n%!";
  let original_termio = set_raw_mode () in
  Printf.printf "Raw mode set. Press keys (Ctrl-C to exit):\n%!";
  Fun.protect
    ~finally:(fun () -> restore_mode original_termio)
    (fun () ->
      let rec loop () = examine (); loop () in
      loop ())
