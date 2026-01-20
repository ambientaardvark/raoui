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
  awaiting_response = false;
  backend_response = None;
  repl_output = None;
  repl_cursor = (0, 1);
  scroll_amount = 0;
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

let test_submit_basic () =
  let width = 10 in
  let state = initial_state width in
  let state = insert_many state width 5 in

  Alcotest.(check string) "lines before submit" "aaaaa" (List.hd state.lines);

  match Update.submit state with
  | Submit (text, new_state) ->
    Alcotest.(check string) "submitted text" "aaaaa" text;
    Alcotest.(check bool) "awaiting_response" true new_state.awaiting_response;
    Alcotest.(check string) "lines reset" "" (List.hd new_state.lines);
    Alcotest.(check int) "cursor_row reset" 0 new_state.cursor_row;
    Alcotest.(check int) "cursor_col reset" 0 new_state.cursor_col
  | _ -> Alcotest.fail "Expected Submit result"

let test_submit_prompt_position () =
  let width = 10 in
  let state = { (initial_state width) with prompt_top_row = 5 } in
  let state = insert_many state width 5 in

  (* Single line of 5 chars, so 1 row *)
  match Update.submit state with
  | Submit (_, new_state) ->
    (* repl_cursor should be at row after the prompt box *)
    let (repl_row, _) = new_state.repl_cursor in
    Alcotest.(check int) "repl_cursor row" 6 repl_row;
    (* prompt_top_row should be one below repl_cursor to leave room for output *)
    Alcotest.(check int) "prompt_top_row" 7 new_state.prompt_top_row
  | _ -> Alcotest.fail "Expected Submit result"

let test_process_response_complete () =
  let width = 10 in
  let state = { (initial_state width) with
    backend_response = Some (Backend.Complete "hello");
    awaiting_response = true;
  } in

  let new_state = Update.process_response state in

  Alcotest.(check (option string)) "repl_output" (Some "hello") new_state.repl_output;
  Alcotest.(check bool) "awaiting_response" false new_state.awaiting_response;
  Alcotest.(check bool) "backend_response cleared" true (new_state.backend_response = None)

let test_process_response_partial () =
  let width = 10 in
  let state = { (initial_state width) with
    backend_response = Some (Backend.Partial "partial");
    awaiting_response = true;
  } in

  let new_state = Update.process_response state in

  Alcotest.(check (option string)) "repl_output" (Some "partial") new_state.repl_output;
  Alcotest.(check bool) "awaiting_response stays true" true new_state.awaiting_response

let test_scroll_when_cursor_below_screen () =
  let width = 10 in
  (* term_height = 10, but prompt starts at row 15 (below screen) *)
  let state = { (initial_state width) with
    prompt_top_row = 15;
    term_height = 10;
  } in

  (* Send any key to trigger universal_corrections *)
  match Update.update (Tty_listener.Char 'a') ~term_width:width state with
  | Continue new_state ->
    (* scroll_amount should be negative (scroll up) to bring cursor into view *)
    (* cursor is at row 15, need to scroll up by 5 to get to row 10 *)
    Alcotest.(check int) "scroll_amount" (-5) new_state.scroll_amount;
    Alcotest.(check int) "prompt_top_row adjusted" 10 new_state.prompt_top_row
  | _ -> Alcotest.fail "Expected Continue result"

let test_submit_scrolls_when_at_bottom () =
  let width = 10 in
  (* prompt at row 10, term_height = 10, single line of text *)
  let state = { (initial_state width) with
    prompt_top_row = 10;
    term_height = 10;
  } in
  let state = insert_many state width 3 in

  match Update.submit state with
  | Submit (_, new_state) ->
    (* output_row would be 11, new_prompt_top would be 12 *)
    (* need to scroll up by 2 to fit *)
    Alcotest.(check int) "scroll_amount" (-2) new_state.scroll_amount;
    Alcotest.(check int) "prompt_top_row" 10 new_state.prompt_top_row;
    let (repl_row, _) = new_state.repl_cursor in
    Alcotest.(check int) "repl_cursor row" 9 repl_row
  | _ -> Alcotest.fail "Expected Submit result"

let test_process_response_clears_scroll () =
  let width = 10 in
  let state = { (initial_state width) with
    backend_response = Some (Backend.Complete "hello");
    scroll_amount = (-5);
  } in

  let new_state = Update.process_response state in

  Alcotest.(check int) "scroll_amount cleared" 0 new_state.scroll_amount

let () =
  let open Alcotest in
  run "Raoui" [
    "wrapping", [
      test_case "Crash on wrap" `Quick test_wrap_crash;
      test_case "Exact width wrap" `Quick test_exact_width_wrap;
      test_case "Resize crash" `Quick test_resize_crash;
      test_case "Resize narrow crash" `Quick test_resize_narrower_crash;
    ];
    "submit", [
      test_case "Basic submit" `Quick test_submit_basic;
      test_case "Prompt position after submit" `Quick test_submit_prompt_position;
    ];
    "process_response", [
      test_case "Complete response" `Quick test_process_response_complete;
      test_case "Partial response" `Quick test_process_response_partial;
    ];
    "scrolling", [
      test_case "Scroll when cursor below screen" `Quick test_scroll_when_cursor_below_screen;
      test_case "Submit scrolls when at bottom" `Quick test_submit_scrolls_when_at_bottom;
      test_case "Process response clears scroll" `Quick test_process_response_clears_scroll;
    ];
  ]