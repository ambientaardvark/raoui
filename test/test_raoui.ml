open Raoui
open Frontend_types

(* Helper to extract text from spans for testing *)
let spans_to_text = function
  | None -> None
  | Some spans -> Some (String.concat "" (List.map snd spans))

(* Helper to convert string to Unicode_string, failing on error *)
let us s = match Unicode_string.of_string s with
  | Ok u -> u
  | Error _ -> failwith ("Invalid unicode: " ^ s)

(* Helper to get first line as string for assertions *)
let first_line_str model = Unicode_string.to_string (List.hd model.lines)

let initial_model width =
  {
    lines = [ Unicode_string.empty ];
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
    prompt_history = [];
    original_prompt = None;
    place_in_history = 0;
    flipping_through_history = None;
  }

let insert_many model n =
  let rec loop s n =
    if n = 0 then s
    else
      let res = Update.update (Tty_listener.Char 'a') s in
      match res with Continue s' -> loop s' (n - 1) | _ -> s
  in
  loop model n

let test_wrap_crash () =
  let width = 10 in
  (* prompt is "> " so length 2. effective = 8 *)
  let model = initial_model width in

  (* Insert 17 chars. *)
  let model = insert_many model 17 in

  Alcotest.(check int) "cursor row after insertion" 2 model.cursor_row;
  Alcotest.(check int) "cursor col after insertion" 1 model.cursor_col;

  (* Now try to move cursor. *)
  let res_left = Update.update Tty_listener.Left model in
  match res_left with
  | Continue _ -> Alcotest.(check bool) "move left success" true true
  | _ -> Alcotest.fail "Failed to move left"

let test_exact_width_wrap () =
  let width = 10 in
  (* effective 8 *)
  let model = initial_model width in
  let model = insert_many model 8 in

  let wrapped = wrap_lines (effective_width model) model.lines in
  let total_rows = List.length wrapped in

  Alcotest.(check int) "exact width rows" 2 total_rows;
  Alcotest.(check int) "cursor row" 1 model.cursor_row;
  Alcotest.(check int) "cursor col" 0 model.cursor_col

let test_resize_crash () =
  let width = 10 in
  let model = initial_model width in
  (* prompt "> ", effective width 8 *)
  (* Insert 9 chars 'a'. "aaaaaaaaa" *)
  let model = insert_many model 9 in

  (* Check assumption: wrapped to 2 lines. Cursor at row 1, col 1. *)
  Alcotest.(check int) "cursor row before resize" 1 model.cursor_row;

  (* Resize to 20. Effective 18. *)
  let new_width = 20 in

  (* The crash happens when we try to update with new width. *)
  (* Specifically when inserting a char or doing an action that uses the old cursor position *)
  try
    let res =
      Update.update (Tty_listener.Char 'b')
        { model with term_width = new_width }
    in
    match res with
    | Continue _ -> Alcotest.(check bool) "Resize and insert success" true true
    | _ -> Alcotest.fail "Unexpected result"
  with Invalid_argument s ->
    Alcotest.fail ("Crashed with Invalid_argument: " ^ s)

let test_resize_narrower_crash () =
  let width = 20 in
  let model = initial_model width in
  (* prompt "> ", effective width 18 *)
  (* Insert 15 chars 'a'. No wrap yet. *)
  let model = insert_many model 15 in

  Alcotest.(check int) "cursor row before narrowing" 0 model.cursor_row;

  (* Resize to 10. Effective 8. *)
  let new_width = 10 in

  try
    let res =
      Update.update (Tty_listener.Char 'b')
        { model with term_width = new_width }
    in
    match res with
    | Continue _ ->
        Alcotest.(check bool) "Resize narrow and insert success" true true
    | _ -> Alcotest.fail "Unexpected result"
  with Invalid_argument s ->
    Alcotest.fail ("Crashed with Invalid_argument on narrowing: " ^ s)

let test_submit_basic () =
  let width = 10 in
  let model = initial_model width in
  let model = insert_many model 5 in

  Alcotest.(check string) "lines before submit" "aaaaa" (first_line_str model);

  match Update.submit model with
  | Submit (text, new_model) ->
      Alcotest.(check string) "submitted text" "aaaaa" text;
      Alcotest.(check bool) "awaiting_response" true new_model.awaiting_response;
      Alcotest.(check string) "lines reset" "" (first_line_str new_model);
      Alcotest.(check int) "cursor_row reset" 0 new_model.cursor_row;
      Alcotest.(check int) "cursor_col reset" 0 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Submit result"

let test_submit_prompt_position () =
  let width = 10 in
  let model = { (initial_model width) with prompt_top_row = 5 } in
  let model = insert_many model 5 in

  (* Single line of 5 chars, so 1 row *)
  match Update.submit model with
  | Submit (_, new_model) ->
      (* repl_cursor should be at row after the prompt box *)
      let repl_row, _ = new_model.repl_cursor in
      Alcotest.(check int) "repl_cursor row" 6 repl_row;
      (* prompt_top_row should be one below repl_cursor to leave room for output *)
      Alcotest.(check int) "prompt_top_row" 7 new_model.prompt_top_row
  | _ -> Alcotest.fail "Expected Submit result"

let test_process_response_done () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      backend_response = Some Backend.Done;
      awaiting_response = true;
    }
  in

  let new_model = Update.process_response model in

  Alcotest.(check (option string))
    "repl_output" (Some "")
    (spans_to_text new_model.repl_output);
  Alcotest.(check bool) "awaiting_response" false new_model.awaiting_response;
  Alcotest.(check bool)
    "backend_response cleared" true
    (new_model.backend_response = None)

let test_process_response_stdout () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      backend_response = Some (Backend.Stdout "hello\n");
      awaiting_response = true;
    }
  in

  let new_model = Update.process_response model in

  Alcotest.(check (option string))
    "repl_output" (Some "hello\n")
    (spans_to_text new_model.repl_output);
  Alcotest.(check bool)
    "awaiting_response stays true" true new_model.awaiting_response

let test_process_response_result () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      backend_response = Some (Backend.Result "[1] 42");
      awaiting_response = true;
    }
  in

  let new_model = Update.process_response model in

  Alcotest.(check (option string))
    "repl_output" (Some "[1] 42")
    (spans_to_text new_model.repl_output);
  Alcotest.(check bool)
    "awaiting_response stays true" true new_model.awaiting_response

let test_process_response_r_error () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      backend_response = Some (Backend.R_error "Error: object 'x' not found");
      awaiting_response = true;
    }
  in

  let new_model = Update.process_response model in

  Alcotest.(check (option string))
    "repl_output" (Some "Error: object 'x' not found")
    (spans_to_text new_model.repl_output);
  (* KEY: R_error is NOT terminal - we keep awaiting_response=true until Done *)
  Alcotest.(check bool)
    "awaiting_response stays true after R_error" true
    new_model.awaiting_response

let test_process_response_internal_error () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      backend_response = Some (Backend.Internal_error "kernel crashed");
      awaiting_response = true;
    }
  in

  let new_model = Update.process_response model in

  Alcotest.(check (option string))
    "repl_output" (Some "Internal error: kernel crashed")
    (spans_to_text new_model.repl_output);
  (* Internal_error IS terminal *)
  Alcotest.(check bool)
    "awaiting_response false after Internal_error" false
    new_model.awaiting_response

let test_scroll_when_cursor_below_screen () =
  let width = 10 in
  (* term_height = 10, but prompt starts at row 15 (below screen) *)
  let model =
    { (initial_model width) with prompt_top_row = 15; term_height = 10 }
  in

  (* Send any key to trigger universal_corrections *)
  match Update.update (Tty_listener.Char 'a') model with
  | Continue new_model ->
      (* scroll_amount should be negative (scroll up) to bring cursor into view *)
      (* cursor is at row 15, need to scroll up by 5 to get to row 10 *)
      Alcotest.(check int) "scroll_amount" (-5) new_model.scroll_amount;
      Alcotest.(check int) "prompt_top_row adjusted" 10 new_model.prompt_top_row
  | _ -> Alcotest.fail "Expected Continue result"

let test_submit_scrolls_when_at_bottom () =
  let width = 10 in
  (* prompt at row 10, term_height = 10, single line of text *)
  let model =
    { (initial_model width) with prompt_top_row = 10; term_height = 10 }
  in
  let model = insert_many model 3 in

  match Update.submit model with
  | Submit (_, new_model) ->
      (* output_row would be 11, new_prompt_top would be 12 *)
      (* need to scroll up by 2 to fit *)
      Alcotest.(check int) "scroll_amount" (-2) new_model.scroll_amount;
      Alcotest.(check int) "prompt_top_row" 10 new_model.prompt_top_row;
      let repl_row, _ = new_model.repl_cursor in
      Alcotest.(check int) "repl_cursor row" 9 repl_row
  | _ -> Alcotest.fail "Expected Submit result"

let test_process_response_clears_scroll () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      backend_response = Some Backend.Done;
      scroll_amount = -5;
    }
  in

  let new_model = Update.process_response model in

  Alcotest.(check int) "scroll_amount cleared" 0 new_model.scroll_amount

(* This test documents the bug we're fixing:
   After an R_error, the state machine should continue until Done.
   Previously, Error was terminal which caused response desync. *)
let test_r_error_followed_by_done () =
  let width = 10 in

  (* Simulate receiving R_error *)
  let model =
    {
      (initial_model width) with
      backend_response = Some (Backend.R_error "Error: oops");
      awaiting_response = true;
    }
  in
  let model = Update.process_response model in

  (* After R_error, we should STILL be awaiting (this is the fix) *)
  Alcotest.(check bool)
    "still awaiting after R_error" true model.awaiting_response;

  (* Then Done arrives *)
  let model = { model with backend_response = Some Backend.Done } in
  let model = Update.process_response model in

  (* NOW we're done awaiting *)
  Alcotest.(check bool) "not awaiting after Done" false model.awaiting_response

(* Helper to convert wrapped result to string list for testing *)
let to_strings us_list = List.map Unicode_string.to_string us_list

(* wrap_line tests *)
(* Note: wrap_line adds an empty string when a line exactly fills width,
   which is needed for cursor positioning at end of full lines *)
let test_wrap_line_short () =
  let result = wrap_line 10 (us "hello") in
  Alcotest.(check (list string)) "short line" [ "hello" ] (to_strings result)

let test_wrap_line_exact () =
  let result = wrap_line 5 (us "hello") in
  (* Exact fit adds empty string for cursor positioning *)
  Alcotest.(check (list string)) "exact fit" [ "hello"; "" ] (to_strings result)

let test_wrap_line_overflow () =
  let result = wrap_line 5 (us "helloworld") in
  (* 10 chars = 2 full rows, adds empty for cursor *)
  Alcotest.(check (list string))
    "overflow wraps" [ "hello"; "world"; "" ] (to_strings result)

let test_wrap_line_multiple () =
  let result = wrap_line 3 (us "abcdefghi") in
  (* 9 chars = 3 full rows, adds empty for cursor *)
  Alcotest.(check (list string))
    "multiple wraps"
    [ "abc"; "def"; "ghi"; "" ]
    (to_strings result)

let test_wrap_line_empty () =
  let result = wrap_line 10 Unicode_string.empty in
  Alcotest.(check (list string)) "empty line" [ "" ] (to_strings result)

let test_wrap_lines_multiline () =
  let result = wrap_lines 5 [ us "hello"; us "world" ] in
  (* Both lines exactly fill width *)
  Alcotest.(check (list string)) "multiline" [ "hello"; ""; "world"; "" ] (to_strings result)

let test_wrap_lines_mixed () =
  let result = wrap_lines 5 [ us "hi"; us "helloworld" ] in
  (* "hi" short, "helloworld" = 2 full rows *)
  Alcotest.(check (list string))
    "mixed lengths"
    [ "hi"; "hello"; "world"; "" ]
    (to_strings result)

(* Coordinate conversion tests *)
let test_internal_to_terminal_simple () =
  let lines = [ us "hello" ] in
  let row, col = internal_to_terminal 10 lines (0, 3) in
  Alcotest.(check int) "row" 0 row;
  Alcotest.(check int) "col" 3 col

let test_internal_to_terminal_wrapped () =
  let lines = [ us "helloworld" ] in
  (* wraps at width 5 *)
  let row, col = internal_to_terminal 5 lines (0, 7) in
  (* col 7 is in second wrapped row, position 2 *)
  Alcotest.(check int) "row in wrapped" 1 row;
  Alcotest.(check int) "col in wrapped" 2 col

let test_internal_to_terminal_multiline () =
  let lines = [ us "hello"; us "world" ] in
  let row, col = internal_to_terminal 10 lines (1, 2) in
  Alcotest.(check int) "row second line" 1 row;
  Alcotest.(check int) "col second line" 2 col

let test_internal_to_terminal_multiline_wrapped () =
  let lines = [ us "helloworld"; us "abc" ] in
  (* first line wraps to 3 rows: "hello", "world", "" *)
  let row, col = internal_to_terminal 5 lines (1, 1) in
  (* first line takes rows 0-2 (including empty), second line starts at row 3 *)
  Alcotest.(check int) "row after wrapped" 3 row;
  Alcotest.(check int) "col after wrapped" 1 col

let test_terminal_to_internal_simple () =
  let lines = [ us "hello" ] in
  let line_idx, col = terminal_to_internal 10 lines (0, 3) in
  Alcotest.(check int) "line_idx" 0 line_idx;
  Alcotest.(check int) "col" 3 col

let test_terminal_to_internal_wrapped () =
  let lines = [ us "helloworld" ] in
  let line_idx, col = terminal_to_internal 5 lines (1, 2) in
  (* row 1 col 2 -> internal col 7 (5 + 2) *)
  Alcotest.(check int) "line_idx wrapped" 0 line_idx;
  Alcotest.(check int) "col wrapped" 7 col

let test_terminal_to_internal_multiline () =
  let lines = [ us "hello"; us "world" ] in
  let line_idx, col = terminal_to_internal 10 lines (1, 2) in
  Alcotest.(check int) "line_idx second" 1 line_idx;
  Alcotest.(check int) "col second" 2 col

(* Coordinate roundtrip *)
let test_coordinate_roundtrip () =
  let lines = [ us "helloworld"; us "foo"; us "barbarbar" ] in
  let width = 5 in
  let test_roundtrip (orig_line, orig_col) =
    let term_row, term_col =
      internal_to_terminal width lines (orig_line, orig_col)
    in
    let line_idx, col = terminal_to_internal width lines (term_row, term_col) in
    Alcotest.(check int)
      (Printf.sprintf "roundtrip line (%d,%d)" orig_line orig_col)
      orig_line line_idx;
    Alcotest.(check int)
      (Printf.sprintf "roundtrip col (%d,%d)" orig_line orig_col)
      orig_col col
  in
  test_roundtrip (0, 0);
  test_roundtrip (0, 5);
  test_roundtrip (0, 9);
  test_roundtrip (1, 0);
  test_roundtrip (1, 2);
  test_roundtrip (2, 0);
  test_roundtrip (2, 8)

(* Awaiting response behavior tests *)
let test_typing_while_awaiting () =
  let width = 10 in
  let model = { (initial_model width) with awaiting_response = true } in

  match Update.update (Tty_listener.Char 'a') model with
  | Continue new_model ->
      Alcotest.(check string) "char inserted" "a" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue with char inserted"

let test_submit_blocked_while_awaiting () =
  let width = 10 in
  let model =
    { (initial_model width) with awaiting_response = true; lines = [ us "test" ] }
  in

  match Update.update (Tty_listener.Ctrl 'p') model with
  | Continue new_model ->
      (* Submit should be blocked, model unchanged *)
      Alcotest.(check string) "lines unchanged" "test" (first_line_str new_model);
      Alcotest.(check bool) "still awaiting" true new_model.awaiting_response
  | Submit _ -> Alcotest.fail "Submit should be blocked while awaiting"
  | _ -> Alcotest.fail "Expected Continue"

let test_cancel_while_awaiting () =
  let width = 10 in
  let model = { (initial_model width) with awaiting_response = true } in

  match Update.update (Tty_listener.Ctrl 'c') model with
  | Cancel -> Alcotest.(check bool) "cancel works" true true
  | _ -> Alcotest.fail "Expected Cancel"

let test_backspace_while_awaiting () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      awaiting_response = true;
      lines = [ us "ab" ];
      cursor_col = 2;
    }
  in

  match Update.update Tty_listener.Backspace model with
  | Continue new_model ->
      Alcotest.(check string) "backspace works" "a" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

(* Cursor movement edge cases *)
let test_left_at_start () =
  let width = 10 in
  let model = initial_model width in

  match Update.update Tty_listener.Left model with
  | Continue new_model ->
      Alcotest.(check int) "col stays 0" 0 new_model.cursor_col;
      Alcotest.(check int) "row stays 0" 0 new_model.cursor_row
  | _ -> Alcotest.fail "Expected Continue"

let test_right_at_end () =
  let width = 10 in
  let model = { (initial_model width) with lines = [ us "ab" ]; cursor_col = 2 } in

  match Update.update Tty_listener.Right model with
  | Continue new_model ->
      Alcotest.(check int) "col stays at end" 2 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_backspace_at_start () =
  let width = 10 in
  let model = initial_model width in

  match Update.update Tty_listener.Backspace model with
  | Continue new_model ->
      Alcotest.(check string)
        "empty line unchanged" "" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_backspace_merges_lines () =
  let width = 10 in
  let model =
    {
      (initial_model width) with
      lines = [ us "hello"; us "world" ];
      cursor_row = 1;
      cursor_col = 0;
    }
  in

  match Update.update Tty_listener.Backspace model with
  | Continue new_model ->
      Alcotest.(check int) "lines merged" 1 (List.length new_model.lines);
      Alcotest.(check string)
        "content merged" "helloworld" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_newline_splits_line () =
  let width = 10 in
  let model =
    { (initial_model width) with lines = [ us "helloworld" ]; cursor_col = 5 }
  in

  match Update.update (Tty_listener.Ctrl 'l') model with
  | Continue new_model ->
      Alcotest.(check int) "two lines" 2 (List.length new_model.lines);
      Alcotest.(check (list string)) "split content" [ "hello"; "world" ] (to_strings new_model.lines)
  | _ -> Alcotest.fail "Expected Continue"

let test_ctrl_d_exit_on_empty () =
  let width = 10 in
  let model = initial_model width in

  match Update.update (Tty_listener.Ctrl 'd') model with
  | Exit -> Alcotest.(check bool) "exits on empty" true true
  | _ -> Alcotest.fail "Expected Exit"

let test_ctrl_d_deletes_char () =
  let width = 10 in
  let model = { (initial_model width) with lines = [ us "ab" ]; cursor_col = 0 } in

  match Update.update (Tty_listener.Ctrl 'd') model with
  | Continue new_model ->
      Alcotest.(check string) "char deleted" "b" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_submit_multiline () =
  let width = 20 in
  let model =
    {
      (initial_model width) with
      lines = [ us "c(1,"; us "2,"; us "3)" ];
      cursor_row = 2;
      cursor_col = 2;
    }
  in

  match Update.submit model with
  | Submit (text, new_model) ->
      Alcotest.(check string) "submitted text" "c(1,\n2,\n3)" text;
      Alcotest.(check bool) "awaiting_response" true new_model.awaiting_response;
      Alcotest.(check string) "lines reset" "" (first_line_str new_model);
      Alcotest.(check int) "lines count" 1 (List.length new_model.lines)
  | _ -> Alcotest.fail "Expected Submit result"

let test_paste_simple () =
  let width = 40 in
  let model = initial_model width in
  match Update.update (Tty_listener.Paste "hello") model with
  | Continue new_model ->
      Alcotest.(check (list string)) "lines" [ "hello" ] (to_strings new_model.lines);
      Alcotest.(check int) "cursor col" 5 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_paste_multiline () =
  let width = 40 in
  let model = initial_model width in
  match Update.update (Tty_listener.Paste "line1\nline2\nline3") model with
  | Continue new_model ->
      Alcotest.(check (list string))
        "lines"
        [ "line1"; "line2"; "line3" ]
        (to_strings new_model.lines);
      Alcotest.(check int) "cursor row" 2 new_model.cursor_row;
      Alcotest.(check int) "cursor col" 5 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_paste_at_cursor () =
  let width = 40 in
  let model =
    { (initial_model width) with lines = [ us "helloworld" ]; cursor_col = 5 }
  in
  match Update.update (Tty_listener.Paste "XXX") model with
  | Continue new_model ->
      Alcotest.(check (list string)) "lines" [ "helloXXXworld" ] (to_strings new_model.lines);
      Alcotest.(check int) "cursor col" 8 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_paste_multiline_at_cursor () =
  let width = 40 in
  let model =
    { (initial_model width) with lines = [ us "helloworld" ]; cursor_col = 5 }
  in
  match Update.update (Tty_listener.Paste "A\nB\nC") model with
  | Continue new_model ->
      Alcotest.(check (list string))
        "lines"
        [ "helloA"; "B"; "Cworld" ]
        (to_strings new_model.lines);
      Alcotest.(check int) "cursor row" 2 new_model.cursor_row;
      Alcotest.(check int) "cursor col" 1 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_paste_truncates_large () =
  let width = 40 in
  let model = initial_model width in
  let large_text = String.make 10000 'x' in
  match Update.update (Tty_listener.Paste large_text) model with
  | Continue new_model ->
      let total_len =
        List.fold_left ( + ) 0 (List.map Unicode_string.byte_length new_model.lines)
      in
      Alcotest.(check bool) "truncated to 5kb" true (total_len <= 5 * 1024)
  | _ -> Alcotest.fail "Expected Continue"

let () =
  let open Alcotest in
  run "Raoui"
    [
      ( "wrap_line",
        [
          test_case "Short line" `Quick test_wrap_line_short;
          test_case "Exact fit" `Quick test_wrap_line_exact;
          test_case "Overflow" `Quick test_wrap_line_overflow;
          test_case "Multiple wraps" `Quick test_wrap_line_multiple;
          test_case "Empty line" `Quick test_wrap_line_empty;
          test_case "Multiline" `Quick test_wrap_lines_multiline;
          test_case "Mixed lengths" `Quick test_wrap_lines_mixed;
        ] );
      ( "coordinate_conversion",
        [
          test_case "Internal to terminal simple" `Quick
            test_internal_to_terminal_simple;
          test_case "Internal to terminal wrapped" `Quick
            test_internal_to_terminal_wrapped;
          test_case "Internal to terminal multiline" `Quick
            test_internal_to_terminal_multiline;
          test_case "Internal to terminal multiline wrapped" `Quick
            test_internal_to_terminal_multiline_wrapped;
          test_case "Terminal to internal simple" `Quick
            test_terminal_to_internal_simple;
          test_case "Terminal to internal wrapped" `Quick
            test_terminal_to_internal_wrapped;
          test_case "Terminal to internal multiline" `Quick
            test_terminal_to_internal_multiline;
          test_case "Coordinate roundtrip" `Quick test_coordinate_roundtrip;
        ] );
      ( "wrapping",
        [
          test_case "Crash on wrap" `Quick test_wrap_crash;
          test_case "Exact width wrap" `Quick test_exact_width_wrap;
          test_case "Resize crash" `Quick test_resize_crash;
          test_case "Resize narrow crash" `Quick test_resize_narrower_crash;
        ] );
      ( "awaiting_response",
        [
          test_case "Typing while awaiting" `Quick test_typing_while_awaiting;
          test_case "Submit blocked while awaiting" `Quick
            test_submit_blocked_while_awaiting;
          test_case "Cancel while awaiting" `Quick test_cancel_while_awaiting;
          test_case "Backspace while awaiting" `Quick
            test_backspace_while_awaiting;
        ] );
      ( "cursor_movement",
        [
          test_case "Left at start" `Quick test_left_at_start;
          test_case "Right at end" `Quick test_right_at_end;
        ] );
      ( "editing",
        [
          test_case "Backspace at start" `Quick test_backspace_at_start;
          test_case "Backspace merges lines" `Quick test_backspace_merges_lines;
          test_case "Newline splits line" `Quick test_newline_splits_line;
          test_case "Ctrl-D exits on empty" `Quick test_ctrl_d_exit_on_empty;
          test_case "Ctrl-D deletes char" `Quick test_ctrl_d_deletes_char;
        ] );
      ( "submit",
        [
          test_case "Basic submit" `Quick test_submit_basic;
          test_case "Prompt position after submit" `Quick
            test_submit_prompt_position;
          test_case "Multiline submit" `Quick test_submit_multiline;
        ] );
      ( "process_response",
        [
          test_case "Done response" `Quick test_process_response_done;
          test_case "Stdout response" `Quick test_process_response_stdout;
          test_case "Result response" `Quick test_process_response_result;
          test_case "R_error response" `Quick test_process_response_r_error;
          test_case "Internal_error response" `Quick
            test_process_response_internal_error;
          test_case "R_error followed by Done" `Quick
            test_r_error_followed_by_done;
        ] );
      ( "scrolling",
        [
          test_case "Scroll when cursor below screen" `Quick
            test_scroll_when_cursor_below_screen;
          test_case "Submit scrolls when at bottom" `Quick
            test_submit_scrolls_when_at_bottom;
          test_case "Process response clears scroll" `Quick
            test_process_response_clears_scroll;
        ] );
      ( "paste",
        [
          test_case "Simple paste" `Quick test_paste_simple;
          test_case "Multiline paste" `Quick test_paste_multiline;
          test_case "Paste at cursor" `Quick test_paste_at_cursor;
          test_case "Multiline paste at cursor" `Quick
            test_paste_multiline_at_cursor;
          test_case "Large paste truncated" `Quick test_paste_truncates_large;
        ] );
    ]
