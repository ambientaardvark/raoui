open Raoui.Tty_listener

let set_raw_mode () =
  let termio = Unix.tcgetattr Unix.stdin in
  let raw_termio =
    {
      termio with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
    }
  in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw_termio;
  termio

let restore_mode termio = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

let examine ~clock ~stdin =
  let desc =
    match await_input ~clock ~stdin with
    | Char key -> Printf.sprintf "Char %S (bytes: %s)" key
        (String.to_seq key |> Seq.map (fun c -> Printf.sprintf "0x%02x" (Char.code c))
         |> List.of_seq |> String.concat " ")
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
    | Other s -> Printf.sprintf "Other: %S" s
    | Unknown s -> Printf.sprintf "Unknown: %S" s
  in
  Printf.printf "%s\n%!" desc

let () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let stdin = Eio.Stdenv.stdin env in
  Printf.printf "Starting listener test...\n%!";
  let original_termio = set_raw_mode () in
  Printf.printf "Raw mode set. Press keys (Ctrl-C to exit):\n%!";
  Fun.protect
    ~finally:(fun () -> restore_mode original_termio)
    (fun () ->
      let rec loop () =
        examine ~clock ~stdin;
        loop ()
      in
      loop ())
