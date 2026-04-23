open Alcotest

let queue_of_string s =
  let q = Queue.create () in
  String.iter (fun c -> Queue.push c q) s;
  q

let with_clock f =
  Eio_main.run (fun env -> f (Eio.Stdenv.clock env))

let test_prefetched_bracketed_paste_decodes () =
  let prefetched = queue_of_string "\x1b[200~x <- 1\x1b[201~" in
  let key =
    with_clock (fun clock ->
        Raoui.Tty_listener.await_input ~prefetched:(Some prefetched) ~clock
          ~stdin:(Eio.Flow.string_source ""))
  in
  match key with
  | Raoui.Tty_listener.Paste text -> check string "paste contents" "x <- 1" text
  | _ -> fail "expected bracketed paste from prefetched bytes"

let test_prefetched_chars_are_read_in_order () =
  let prefetched = queue_of_string "abc" in
  let keys =
    with_clock (fun clock ->
        let stdin = Eio.Flow.string_source "" in
        let a =
          Raoui.Tty_listener.await_input ~prefetched:(Some prefetched) ~clock
            ~stdin
        in
        let b =
          Raoui.Tty_listener.await_input ~prefetched:(Some prefetched) ~clock
            ~stdin
        in
        let c =
          Raoui.Tty_listener.await_input ~prefetched:(Some prefetched) ~clock
            ~stdin
        in
        [ a; b; c ])
  in
  let pp_key fmt = function
    | Raoui.Tty_listener.Char s -> Format.fprintf fmt "Char %S" s
    | Raoui.Tty_listener.Paste s -> Format.fprintf fmt "Paste %S" s
    | Raoui.Tty_listener.Enter -> Format.fprintf fmt "Enter"
    | Raoui.Tty_listener.Tab -> Format.fprintf fmt "Tab"
    | Raoui.Tty_listener.Backspace -> Format.fprintf fmt "Backspace"
    | Raoui.Tty_listener.Escape -> Format.fprintf fmt "Escape"
    | Raoui.Tty_listener.Up -> Format.fprintf fmt "Up"
    | Raoui.Tty_listener.Down -> Format.fprintf fmt "Down"
    | Raoui.Tty_listener.Left -> Format.fprintf fmt "Left"
    | Raoui.Tty_listener.Right -> Format.fprintf fmt "Right"
    | Raoui.Tty_listener.Home -> Format.fprintf fmt "Home"
    | Raoui.Tty_listener.End -> Format.fprintf fmt "End"
    | Raoui.Tty_listener.Delete -> Format.fprintf fmt "Delete"
    | Raoui.Tty_listener.Ctrl c -> Format.fprintf fmt "Ctrl %C" c
    | Raoui.Tty_listener.Other s -> Format.fprintf fmt "Other %S" s
    | Raoui.Tty_listener.Unknown s -> Format.fprintf fmt "Unknown %S" s
  in
  let key = testable pp_key ( = ) in
  check (list key) "drains queued characters"
    [
      Raoui.Tty_listener.Char "a";
      Raoui.Tty_listener.Char "b";
      Raoui.Tty_listener.Char "c";
    ]
    keys

let test_prefetched_ss3_left_arrow_decodes () =
  let prefetched = queue_of_string "\x1bOD" in
  let key =
    with_clock (fun clock ->
        Raoui.Tty_listener.await_input ~prefetched:(Some prefetched) ~clock
          ~stdin:(Eio.Flow.string_source ""))
  in
  check bool "ss3 left arrow decodes" true
    (key = Raoui.Tty_listener.Left)

let test_prefetched_ss3_home_decodes () =
  let prefetched = queue_of_string "\x1bOH" in
  let key =
    with_clock (fun clock ->
        Raoui.Tty_listener.await_input ~prefetched:(Some prefetched) ~clock
          ~stdin:(Eio.Flow.string_source ""))
  in
  check bool "ss3 home decodes" true
    (key = Raoui.Tty_listener.Home)

let () =
  run "tty_listener"
    [
      ( "prefetched",
        [
          test_case "bracketed paste decodes" `Quick
            test_prefetched_bracketed_paste_decodes;
          test_case "plain chars read in order" `Quick
            test_prefetched_chars_are_read_in_order;
          test_case "ss3 left arrow decodes" `Quick
            test_prefetched_ss3_left_arrow_decodes;
          test_case "ss3 home decodes" `Quick test_prefetched_ss3_home_decodes;
        ] );
    ]
