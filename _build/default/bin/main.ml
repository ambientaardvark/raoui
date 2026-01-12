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

(* Model *)
type state = {
  text : string;
  cursor : int;
  output : string option;  (* lines to print above prompt *)
}

let init = { text = ""; cursor = 0; output = None }

(* Update *)
let insert_char state c =
  let before = String.sub state.text 0 state.cursor in
  let after = String.sub state.text state.cursor (String.length state.text - state.cursor) in
  { state with text = before ^ String.make 1 c ^ after; cursor = state.cursor + 1 }

let delete_char state =
  if state.cursor = 0 then state
  else
    let before = String.sub state.text 0 (state.cursor - 1) in
    let after = String.sub state.text state.cursor (String.length state.text - state.cursor) in
    { state with text = before ^ after; cursor = state.cursor - 1 }

let move_left state =
  { state with cursor = max 0 (state.cursor - 1) }

let move_right state =
  { state with cursor = min (String.length state.text) (state.cursor + 1) }

let submit state =
  { text = ""; cursor = 0; output = Some state.text }

let update key state =
  let open Raoui.Tty_listener in
  let state = { state with output = None } in
  match key with
  | Ctrl 'd' -> None
  | Enter -> Some (submit state)
  | Char c -> Some (insert_char state c)
  | Backspace -> Some (delete_char state)
  | Left -> Some (move_left state)
  | Right -> Some (move_right state)
  | _ -> Some state

(* View *)
let view state =
  (match state.output with
   | Some line -> Printf.printf "\n%s\n" line
   | None -> ());
  let prompt = "> " in
  let cursor_col = String.length prompt + state.cursor + 1 in
  Printf.printf "\r\x1b[K%s%s\x1b[%dG%!" prompt state.text cursor_col

(* Main loop *)
let run () =
  let rec loop state =
    view state;
    match update (Raoui.Tty_listener.await_input ()) state with
    | None -> ()
    | Some state -> loop state
  in
  loop init

let () =
  let orig = set_raw_mode () in
  Fun.protect ~finally:(fun () -> print_newline (); restore_mode orig) run
