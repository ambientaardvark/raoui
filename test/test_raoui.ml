open Raoui
open Frontend_types

let initial_state width = {
  lines = [""];
  cursor_row = 0;
  cursor_col = 0;
  prompt_top_row = 0;
  term_width = width;
  term_height = 10;
  prompt_box_height = 1;
  previous_prompt_top_row = 0;
  previous_key = None;
  persistent_col = 0;
}

let insert_many state width n =
  let rec loop s n =
    if n = 0 then s
    else 
      let res = Update.update (Tty_listener.Char 'a') ~term_width:width s in
      match res with
      | Continue s' -> loop s' (n - 1)
      | _ -> s
  in
  loop state n

let test_wrap_crash () =
  let width = 10 in
  (* prompt is "> " so length 2. effective = 8 *)
  let state = initial_state width in
  
  (* Insert 17 chars. *)
  let state = insert_many state width 17 in
  
  Alcotest.(check int) "cursor row after insertion" 2 state.cursor_row;
  Alcotest.(check int) "cursor col after insertion" 1 state.cursor_col;
  
  (* Now try to move cursor. *)
  let res_left = Update.update Tty_listener.Left ~term_width:width state in
  match res_left with
  | Continue _ -> Alcotest.(check bool) "move left success" true true
  | _ -> Alcotest.fail "Failed to move left"

let test_exact_width_wrap () =
  let width = 10 in (* effective 8 *)
  let state = initial_state width in
  let state = insert_many state width 8 in
  
  let wrapped = wrap_lines (effective_width state) state.lines in
  let total_rows = List.length wrapped in
  
  Alcotest.(check int) "exact width rows" 2 total_rows;
  Alcotest.(check int) "cursor row" 1 state.cursor_row;
  Alcotest.(check int) "cursor col" 0 state.cursor_col

let test_resize_crash () =
  let width = 10 in
  let state = initial_state width in
  (* prompt "> ", effective width 8 *)
  (* Insert 9 chars 'a'. "aaaaaaaaa" *)
  let state = insert_many state width 9 in
  
  (* Check assumption: wrapped to 2 lines. Cursor at row 1, col 1. *)
  Alcotest.(check int) "cursor row before resize" 1 state.cursor_row;
  
  (* Resize to 20. Effective 18. *)
  let new_width = 20 in
  
  (* The crash happens when we try to update with new width. *)
  (* Specifically when inserting a char or doing an action that uses the old cursor position *)
  try
    let res = Update.update (Tty_listener.Char 'b') ~term_width:new_width state in
    match res with
    | Continue _ -> Alcotest.(check bool) "Resize and insert success" true true
    | _ -> Alcotest.fail "Unexpected result"
  with Invalid_argument s ->
    Alcotest.fail ("Crashed with Invalid_argument: " ^ s)

let test_resize_narrower_crash () =
  let width = 20 in
  let state = initial_state width in
  (* prompt "> ", effective width 18 *)
  (* Insert 15 chars 'a'. No wrap yet. *)
  let state = insert_many state width 15 in
  
  Alcotest.(check int) "cursor row before narrowing" 0 state.cursor_row;
  
  (* Resize to 10. Effective 8. *)
  let new_width = 10 in
  
  try
    let res = Update.update (Tty_listener.Char 'b') ~term_width:new_width state in
    match res with
    | Continue _ -> Alcotest.(check bool) "Resize narrow and insert success" true true
    | _ -> Alcotest.fail "Unexpected result"
  with Invalid_argument s ->
    Alcotest.fail ("Crashed with Invalid_argument on narrowing: " ^ s)

let () =
  let open Alcotest in
  run "Raoui" [
    "wrapping", [
      test_case "Crash on wrap" `Quick test_wrap_crash;
      test_case "Exact width wrap" `Quick test_exact_width_wrap;
      test_case "Resize crash" `Quick test_resize_crash;
      test_case "Resize narrow crash" `Quick test_resize_narrower_crash;
    ];
  ]