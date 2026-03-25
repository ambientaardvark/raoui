open Raoui
open Frontend_types

module V = View.Make (Terminal_ops.Ansi)

type test_result =
  | Continue of model
  | Submit of string * model
  | Exit
  | Cancel

let classify (m, effects) =
  match effects with
  | [] -> Continue m
  | [Repl_effect.Submit t] -> Submit (t, m)
  | [Repl_effect.Quit] -> Exit
  | [Repl_effect.Cancel] -> Cancel
  | [Repl_effect.RequestCompletions _] -> Continue m
  | [Repl_effect.BackgroundSubmit _] -> Continue m
  | _ -> failwith (Printf.sprintf "unexpected effects: %d" (List.length effects))

let update msg model = classify (Update.update msg model)
let submit model = classify (Update.submit model)

(* Helper to extract text from spans for testing *)
let spans_to_text = function
  | None -> None
  | Some spans -> Some (String.concat "" (List.map snd spans))

(* Helper to convert string to Unicode_string, failing on error *)
let us s =
  match Unicode_string.of_string s with
  | Ok u -> u
  | Error _ -> failwith ("Invalid unicode: " ^ s)

(* Helper to get first line as string for assertions *)
let first_line_str model = Unicode_string.to_string (List.hd model.lines)

let initial_model width =
  let lines = [ Unicode_string.empty ] in
  {
    lines;
    lex_cache = Syntax.Cache.create lines;
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
    prompt_top_row = 0;
    term_width = width;
    term_height = 10;
    prompt_box_height = Frontend_types.min_prompt_height;
    previous_prompt_top_row = 0;
    previous_key = None;
    persistent_col = 0;
    awaiting_response = false;
    backend_response = None;
    repl_output = None;
    repl_cursor = (0, 1);
    scroll_amount = 0;
    history = History.init "/dev/null";
    flipping_through_history = None;
    running_in_ide = false;
    completion = None;
    mode = Frontend_types.Normal;
  }

let with_lines model lines =
  { model with lines; lex_cache = Syntax.Cache.create lines }

let with_cursor_internal model ~line ~pos =
  let row, col =
    internal_to_terminal (effective_width model) model.lines (line, pos)
  in
  {
    model with
    cursor_line = line;
    cursor_pos = pos;
    cursor_row = row;
    cursor_col = col;
  }

let style_to_string = function
  | `Raw -> "Raw"
  | `Plain -> "Plain"
  | `Accent -> "Accent"
  | `Error -> "Error"
  | `Keyword -> "Keyword"
  | `String -> "String"
  | `Number -> "Number"
  | `Comment -> "Comment"
  | `Operator -> "Operator"
  | `Constant -> "Constant"
  | `Ident -> "Ident"
  | `Bracket -> "Bracket"
  | `Function -> "Function"
  | `Completion -> "Completion"
  | `Completion_selected -> "Completion_selected"
  | `Shell_prompt -> "Shell_prompt"

let pp_span fmt (style, text) =
  Format.fprintf fmt "(%s,%S)" (style_to_string style) text

let printed_rows model =
  let model =
    if model.prompt_top_row < 1 then { model with prompt_top_row = 1 } else model
  in
  let rows_rev = ref [] in
  Queue.iter
    (function
      | Terminal_ops.Print spans -> rows_rev := spans :: !rows_rev
      | _ -> ())
    (V.view_ops model);
  List.rev !rows_rev

let view_ops_list model =
  let model =
    if model.prompt_top_row < 1 then { model with prompt_top_row = 1 } else model
  in
  Queue.fold (fun acc op -> op :: acc) [] (V.view_ops model) |> List.rev

let clears_row row ops =
  let rec loop = function
    | Terminal_ops.Cursor_to (r, 1) :: Terminal_ops.Clear_to_eol :: _ when r = row ->
        true
    | _ :: rest -> loop rest
    | [] -> false
  in
  loop ops

let row_content spans =
  match spans with
  | _prompt :: content -> content
  | [] -> []

let is_completion_style = function
  | `Completion | `Completion_selected -> true
  | _ -> false

let completion_rows model =
  printed_rows model
  |> List.filter (function
       | (style, _) :: _ -> is_completion_style style
       | [] -> false)

let completion_row_text spans = String.concat "" (List.map snd spans) |> String.trim

let completion_row_selected = function
  | (`Completion_selected, _) :: _ -> true
  | _ -> false

let spans_of_cache cache =
  List.map
    (fun (l : Syntax.Cache.entry) ->
      List.map (fun t -> Syntax.token_to_span t) l.tokens)
    cache

let check_lex_cache ~msg model =
  let expected = Syntax.Cache.create model.lines |> spans_of_cache in
  let actual = spans_of_cache model.lex_cache in
  let span_testable = Alcotest.testable pp_span ( = ) in
  let spans_testable = Alcotest.list span_testable in
  Alcotest.(check (list spans_testable)) msg expected actual

let insert_many model n =
  let rec loop s n =
    if n = 0 then s
    else
      let s' = match update (Update.Key (Tty_listener.Char "a")) s with
        | Continue m -> m | _ -> s in
      loop s' (n - 1)
  in
  loop model n

let cycle_completion_many cs n =
  let rec loop cs remaining =
    if remaining <= 0 then cs else loop (Completion.cycle_next cs) (remaining - 1)
  in
  loop cs n

let make_completion_items n =
  List.init n (fun i -> Printf.sprintf "item%d" i)

let test_wrap_crash () =
  let width = 10 in
  (* prompt is "> " so length 2. effective = 8 *)
  let model = initial_model width in

  (* Insert 17 chars. *)
  let model = insert_many model 17 in

  Alcotest.(check int) "cursor row after insertion" 2 model.cursor_row;
  Alcotest.(check int) "cursor col after insertion" 1 model.cursor_col;

  (* Now try to move cursor. *)
  let res_left = update (Update.Key Tty_listener.Left) model in
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
      update (Update.Key (Tty_listener.Char "b"))
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
      update (Update.Key (Tty_listener.Char "b"))
        { model with term_width = new_width }
    in
    match res with
    | Continue _ ->
        Alcotest.(check bool) "Resize narrow and insert success" true true
    | _ -> Alcotest.fail "Unexpected result"
  with Invalid_argument s ->
    Alcotest.fail ("Crashed with Invalid_argument on narrowing: " ^ s)

let test_wrapped_syntax_highlighting () =
  let model = with_lines (initial_model 7) [ us "glue(\"abcdef\")" ] in
  let rows = printed_rows model |> List.map row_content in
  Alcotest.(check int) "three wrapped rows" 3 (List.length rows);
  let row0 = List.nth rows 0 in
  let row1 = List.nth rows 1 in
  let row2 = List.nth rows 2 in
  Alcotest.(check (list string))
    "row 0 styles"
    [ "Function"; "Bracket" ]
    (List.map (fun (style, _) -> style_to_string style) row0);
  Alcotest.(check string)
    "row 0 text"
    "glue("
    (String.concat "" (List.map snd row0));
  Alcotest.(check (list string))
    "row 1 styles"
    [ "String" ]
    (List.map (fun (style, _) -> style_to_string style) row1);
  Alcotest.(check string)
    "row 1 text"
    "\"abcd"
    (String.concat "" (List.map snd row1));
  Alcotest.(check (list string))
    "row 2 styles"
    [ "String"; "Bracket" ]
    (List.map (fun (style, _) -> style_to_string style) row2);
  Alcotest.(check string)
    "row 2 text"
    "ef\")"
    (String.concat "" (List.map snd row2))

let test_wrapped_exact_width_keeps_trailing_row () =
  let model = with_lines (initial_model 7) [ us "12345" ] in
  let rows = printed_rows model |> List.map row_content in
  Alcotest.(check int) "exact-width row count" 2 (List.length rows);
  Alcotest.(check (list string))
    "first row styles"
    [ "Number" ]
    (List.map (fun (style, _) -> style_to_string style) (List.nth rows 0));
  Alcotest.(check string)
    "first row text"
    "12345"
    (String.concat "" (List.map snd (List.nth rows 0)));
  Alcotest.(check int) "second row is empty" 0 (List.length (List.nth rows 1))

let test_submit_basic () =
  let width = 10 in
  let model = initial_model width in
  let model = insert_many model 5 in

  Alcotest.(check string) "lines before submit" "aaaaa" (first_line_str model);

  match submit model with
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
  match submit model with
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
      backend_response = Some Ffi_backend.Done;
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
      backend_response = Some (Ffi_backend.Stdout "hello\n");
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
      backend_response = Some (Ffi_backend.Result "[1] 42");
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
      backend_response =
        Some (Ffi_backend.R_error "Error: object 'x' not found");
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
      backend_response = Some (Ffi_backend.Internal_error "kernel crashed");
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
  match update (Update.Key (Tty_listener.Char "a")) model with
  | Continue new_model ->
      (* No physical scroll needed — just reposition the prompt *)
      Alcotest.(check int) "scroll_amount" 0 new_model.scroll_amount;
      Alcotest.(check int) "prompt_top_row adjusted" 10 new_model.prompt_top_row
  | _ -> Alcotest.fail "Expected Continue result"

let test_submit_scrolls_when_at_bottom () =
  let width = 10 in
  (* prompt at row 10, term_height = 10, single line of text *)
  let model =
    { (initial_model width) with prompt_top_row = 10; term_height = 10 }
  in
  let model = insert_many model 3 in

  match submit model with
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
      backend_response = Some Ffi_backend.Done;
      scroll_amount = -5;
    }
  in

  let new_model = Update.process_response model in

  Alcotest.(check int) "scroll_amount cleared" 0 new_model.scroll_amount

let test_view_clears_reserved_output_row_before_first_chunk () =
  let width = 20 in
  let model =
    {
      (initial_model width) with
      prompt_top_row = 5;
      repl_cursor = (4, 1);
      awaiting_response = true;
    }
  in
  Alcotest.(check bool)
    "reserved output row is cleared" true
    (clears_row 4 (view_ops_list model))

let test_view_preserves_streamed_output_row () =
  let width = 20 in
  let model =
    {
      (initial_model width) with
      prompt_top_row = 5;
      repl_cursor = (4, 10);
      awaiting_response = true;
    }
  in
  Alcotest.(check bool)
    "streamed output row is not cleared" false
    (clears_row 4 (view_ops_list model))

(* This test documents the bug we're fixing:
   After an R_error, the state machine should continue until Done.
   Previously, Error was terminal which caused response desync. *)
let test_r_error_followed_by_done () =
  let width = 10 in

  (* Simulate receiving R_error *)
  let model =
    {
      (initial_model width) with
      backend_response = Some (Ffi_backend.R_error "Error: oops");
      awaiting_response = true;
    }
  in
  let model = Update.process_response model in

  (* After R_error, we should STILL be awaiting (this is the fix) *)
  Alcotest.(check bool)
    "still awaiting after R_error" true model.awaiting_response;

  (* Then Done arrives *)
  let model = { model with backend_response = Some Ffi_backend.Done } in
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
  Alcotest.(check (list string))
    "multiline"
    [ "hello"; ""; "world"; "" ]
    (to_strings result)

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

  match update (Update.Key (Tty_listener.Char "a")) model with
  | Continue new_model ->
      Alcotest.(check string) "char inserted" "a" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue with char inserted"

let test_submit_blocked_while_awaiting () =
  let width = 10 in
  let model =
    {
      (with_lines (initial_model width) [ us "test" ]) with
      awaiting_response = true;
    }
  in

  match update (Update.Key (Tty_listener.Ctrl 'p')) model with
  | Continue new_model ->
      (* Submit should be blocked, model unchanged *)
      Alcotest.(check string)
        "lines unchanged" "test" (first_line_str new_model);
      Alcotest.(check bool) "still awaiting" true new_model.awaiting_response
  | Submit _ -> Alcotest.fail "Submit should be blocked while awaiting"
  | _ -> Alcotest.fail "Expected Continue"

let test_cancel_while_awaiting () =
  let width = 10 in
  let model = { (initial_model width) with awaiting_response = true } in

  match update (Update.Key (Tty_listener.Ctrl 'c')) model with
  | Cancel -> Alcotest.(check bool) "cancel works" true true
  | _ -> Alcotest.fail "Expected Cancel"

let test_backspace_while_awaiting () =
  let width = 10 in
  let model =
    with_cursor_internal
      {
        (with_lines (initial_model width) [ us "ab" ]) with
        awaiting_response = true;
      }
      ~line:0 ~pos:2
  in

  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check string) "backspace works" "a" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

(* Cursor movement edge cases *)
let test_left_at_start () =
  let width = 10 in
  let model = initial_model width in

  match update (Update.Key Tty_listener.Left) model with
  | Continue new_model ->
      Alcotest.(check int) "col stays 0" 0 new_model.cursor_col;
      Alcotest.(check int) "row stays 0" 0 new_model.cursor_row
  | _ -> Alcotest.fail "Expected Continue"

let test_right_at_end () =
  let width = 10 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "ab" ])
      ~line:0 ~pos:2
  in

  match update (Update.Key Tty_listener.Right) model with
  | Continue new_model ->
      Alcotest.(check int) "col stays at end" 2 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_cursor_moves_over_wide_grapheme () =
  let width = 10 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "a中b" ])
      ~line:0 ~pos:0
  in
  let model =
    match update (Update.Key Tty_listener.Right) model with
    | Continue new_model -> new_model
    | _ -> Alcotest.fail "Expected Continue after first right"
  in
  Alcotest.(check int) "col after 'a'" 1 model.cursor_col;
  let model =
    match update (Update.Key Tty_listener.Right) model with
    | Continue new_model -> new_model
    | _ -> Alcotest.fail "Expected Continue after second right"
  in
  Alcotest.(check int) "col after '中'" 3 model.cursor_col;
  let model =
    match update (Update.Key Tty_listener.Left) model with
    | Continue new_model -> new_model
    | _ -> Alcotest.fail "Expected Continue after left"
  in
  Alcotest.(check int) "col back over '中'" 1 model.cursor_col

let test_cursor_moves_over_emoji_zwj () =
  let width = 10 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "a👩🏻‍⚕️b" ])
      ~line:0 ~pos:0
  in
  let model =
    match update (Update.Key Tty_listener.Right) model with
    | Continue new_model -> new_model
    | _ -> Alcotest.fail "Expected Continue after first right"
  in
  Alcotest.(check int) "col after 'a'" 1 model.cursor_col;
  let model =
    match update (Update.Key Tty_listener.Right) model with
    | Continue new_model -> new_model
    | _ -> Alcotest.fail "Expected Continue after second right"
  in
  Alcotest.(check int) "col after emoji" 3 model.cursor_col;
  let model =
    match update (Update.Key Tty_listener.Left) model with
    | Continue new_model -> new_model
    | _ -> Alcotest.fail "Expected Continue after left"
  in
  Alcotest.(check int) "col back over emoji" 1 model.cursor_col

let test_backspace_at_start () =
  let width = 10 in
  let model = initial_model width in

  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check string)
        "empty line unchanged" "" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_backspace_merges_lines () =
  let width = 10 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "hello"; us "world" ])
      ~line:1 ~pos:0
  in

  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check int) "lines merged" 1 (List.length new_model.lines);
      Alcotest.(check string)
        "content merged" "helloworld" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_newline_splits_line () =
  let width = 10 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "helloworld" ])
      ~line:0 ~pos:5
  in

  match update (Update.Key (Tty_listener.Ctrl '\r')) model with
  | Continue new_model ->
      Alcotest.(check int) "two lines" 2 (List.length new_model.lines);
      Alcotest.(check (list string))
        "split content" [ "hello"; "world" ]
        (to_strings new_model.lines)
  | _ -> Alcotest.fail "Expected Continue"

let test_ctrl_d_exit_on_empty () =
  let width = 10 in
  let model = initial_model width in

  match update (Update.Key (Tty_listener.Ctrl 'd')) model with
  | Exit -> Alcotest.(check bool) "exits on empty" true true
  | _ -> Alcotest.fail "Expected Exit"

let test_ctrl_d_deletes_char () =
  let width = 10 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "ab" ])
      ~line:0 ~pos:0
  in

  match update (Update.Key (Tty_listener.Ctrl 'd')) model with
  | Continue new_model ->
      Alcotest.(check string) "char deleted" "b" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_submit_multiline () =
  let width = 20 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "c(1,"; us "2,"; us "3)" ])
      ~line:2 ~pos:2
  in

  match submit model with
  | Submit (text, new_model) ->
      Alcotest.(check string) "submitted text" "c(1,\n2,\n3)" text;
      Alcotest.(check bool) "awaiting_response" true new_model.awaiting_response;
      Alcotest.(check string) "lines reset" "" (first_line_str new_model);
      Alcotest.(check int) "lines count" 1 (List.length new_model.lines)
  | _ -> Alcotest.fail "Expected Submit result"

let test_submit_continuation_if_paren () =
  let width = 20 in
  let line = us "if (x)" in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ line ])
      ~line:0
      ~pos:(Unicode_string.length line)
  in
  match submit model with
  | Continue new_model ->
      Alcotest.(check int) "lines count" 2 (List.length new_model.lines);
      Alcotest.(check string)
        "first line preserved" "if (x)" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue for if (x)"

let test_submit_continuation_function_paren () =
  let width = 20 in
  let line = us "function(x)" in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ line ])
      ~line:0
      ~pos:(Unicode_string.length line)
  in
  match submit model with
  | Continue new_model ->
      Alcotest.(check int) "lines count" 2 (List.length new_model.lines);
      Alcotest.(check string)
        "first line preserved" "function(x)" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue for function(x)"

let test_submit_if_braced_single_line () =
  let width = 20 in
  let line = us "if (x) { 1 }" in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ line ])
      ~line:0
      ~pos:(Unicode_string.length line)
  in
  match submit model with
  | Submit (text, _new_model) ->
      Alcotest.(check string) "submitted text" "if (x) { 1 }" text
  | _ -> Alcotest.fail "Expected Submit for if (x) { 1 }"

let test_submit_lambda_body_same_line () =
  let width = 40 in
  let line = us "map_dbl(li, \\(x) x + 1)" in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ line ])
      ~line:0
      ~pos:(Unicode_string.length line)
  in
  match submit model with
  | Submit (text, _new_model) ->
      Alcotest.(check string) "submitted text" "map_dbl(li, \\(x) x + 1)" text
  | _ -> Alcotest.fail "Expected Submit for lambda body on same line"

let test_paste_simple () =
  let width = 40 in
  let model = initial_model width in
  match update (Update.Key (Tty_listener.Paste "hello")) model with
  | Continue new_model ->
      Alcotest.(check (list string))
        "lines" [ "hello" ]
        (to_strings new_model.lines);
      Alcotest.(check int) "cursor col" 5 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_paste_multiline () =
  let width = 40 in
  let model = initial_model width in
  match
    update (Update.Key (Tty_listener.Paste "line1\nline2\nline3")) model
  with
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
    with_cursor_internal
      (with_lines (initial_model width) [ us "helloworld" ])
      ~line:0 ~pos:5
  in
  match update (Update.Key (Tty_listener.Paste "XXX")) model with
  | Continue new_model ->
      Alcotest.(check (list string))
        "lines" [ "helloXXXworld" ]
        (to_strings new_model.lines);
      Alcotest.(check int) "cursor col" 8 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_paste_multiline_at_cursor () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "helloworld" ])
      ~line:0 ~pos:5
  in
  match update (Update.Key (Tty_listener.Paste "A\nB\nC")) model with
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
  match update (Update.Key (Tty_listener.Paste large_text)) model with
  | Continue new_model ->
      let total_len =
        List.fold_left ( + ) 0
          (List.map Unicode_string.byte_length new_model.lines)
      in
      Alcotest.(check bool) "truncated to 5kb" true (total_len <= 5 * 1024)
  | _ -> Alcotest.fail "Expected Continue"

let test_lex_cache_single_line_edit () =
  let width = 80 in
  let line = us "if (x" in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ line ])
      ~line:0
      ~pos:(Unicode_string.length line)
  in
  match update (Update.Key (Tty_listener.Char ")")) model with
  | Continue new_model ->
      check_lex_cache ~msg:"lex cache matches after single-line edit" new_model
  | _ -> Alcotest.fail "Expected Continue"

let test_lex_cache_multiline_mode_change () =
  let width = 80 in
  let line0 = us "\"abc" in
  let line1 = us "def\"" in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ line0; line1 ])
      ~line:0
      ~pos:(Unicode_string.length line0)
  in
  match update (Update.Key (Tty_listener.Char {|"|})) model with
  | Continue new_model ->
      check_lex_cache ~msg:"lex cache matches after multiline edit" new_model
  | _ -> Alcotest.fail "Expected Continue"

let test_lex_delete_empty_line : unit -> unit =
 fun () ->
  let width = 80 in
  let lines = List.map us [ "a"; "b"; ""; "c"; "d" ] in
  let model =
    with_cursor_internal (with_lines (initial_model width) lines) ~line:2 ~pos:0
  in
  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      let lexemes =
        List.concat_map
          (fun (line : Syntax.Cache.entry) ->
            List.map Syntax.token_to_lexeme line.tokens)
          new_model.lex_cache
      in
      Alcotest.(check string) "First letter is a" "a" (List.nth lexemes 0);
      Alcotest.(check string) "Second letter is b" "b" (List.nth lexemes 1);
      Alcotest.(check string) "Third letter is c" "c" (List.nth lexemes 2);
      Alcotest.(check string) "Fourth letter is d" "d" (List.nth lexemes 3)
  | _ -> Alcotest.fail "expected continue"

(* Matched bracket insertion/deletion tests *)
let test_insert_matched_paren () =
  let width = 40 in
  let model = initial_model width in
  match update (Update.Key (Tty_listener.Char "(")) model with
  | Continue new_model ->
      Alcotest.(check string) "inserts pair" "()" (first_line_str new_model);
      Alcotest.(check int) "cursor between" 1 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_insert_matched_bracket () =
  let width = 40 in
  let model = initial_model width in
  match update (Update.Key (Tty_listener.Char "[")) model with
  | Continue new_model ->
      Alcotest.(check string) "inserts pair" "[]" (first_line_str new_model);
      Alcotest.(check int) "cursor between" 1 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_insert_matched_brace () =
  let width = 40 in
  let model = initial_model width in
  match update (Update.Key (Tty_listener.Char "{")) model with
  | Continue new_model ->
      Alcotest.(check string) "inserts pair" "{}" (first_line_str new_model);
      Alcotest.(check int) "cursor between" 1 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_insert_matched_quote () =
  let width = 40 in
  let model = initial_model width in
  match update (Update.Key (Tty_listener.Char {|"|})) model with
  | Continue new_model ->
      Alcotest.(check string) "inserts pair" "\"\"" (first_line_str new_model);
      Alcotest.(check int) "cursor between" 1 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_skip_closing_paren () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "()" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key (Tty_listener.Char ")")) model with
  | Continue new_model ->
      Alcotest.(check string) "no extra char" "()" (first_line_str new_model);
      Alcotest.(check int) "cursor moved past" 2 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_skip_closing_quote () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "\"\"" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key (Tty_listener.Char {|"|})) model with
  | Continue new_model ->
      Alcotest.(check string) "no extra char" "\"\"" (first_line_str new_model);
      Alcotest.(check int) "cursor moved past" 2 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_delete_matched_paren () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "()" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check string) "both deleted" "" (first_line_str new_model);
      Alcotest.(check int) "cursor at start" 0 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_delete_matched_bracket () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "[]" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check string) "both deleted" "" (first_line_str new_model);
      Alcotest.(check int) "cursor at start" 0 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_delete_matched_brace () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "{}" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check string) "both deleted" "" (first_line_str new_model);
      Alcotest.(check int) "cursor at start" 0 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_delete_matched_quote () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "\"\"" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check string) "both deleted" "" (first_line_str new_model);
      Alcotest.(check int) "cursor at start" 0 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_delete_unmatched_paren () =
  let width = 40 in
  (* cursor between ( and x, not a matched pair *)
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "(x)" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key Tty_listener.Backspace) model with
  | Continue new_model ->
      Alcotest.(check string) "only ( deleted" "x)" (first_line_str new_model);
      Alcotest.(check int) "cursor at start" 0 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_matched_insert_with_content () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "abc" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key (Tty_listener.Char "(")) model with
  | Continue new_model ->
      Alcotest.(check string) "pair inserted" "a(bc" (first_line_str new_model);
      Alcotest.(check int) "cursor between" 2 new_model.cursor_col
  | _ -> Alcotest.fail "Expected Continue"

let test_empty_brace_expands_on_enter () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "{}" ])
      ~line:0 ~pos:1
  in
  match update (Update.Key Tty_listener.Enter) model with
  | Continue new_model ->
      Alcotest.(check (list string))
        "expanded lines" [ "{"; "  "; "}" ]
        (to_strings new_model.lines);
      Alcotest.(check int) "cursor line" 1 new_model.cursor_line;
      Alcotest.(check int) "cursor pos" 2 new_model.cursor_pos
  | _ -> Alcotest.fail "Expected Continue"

let test_brace_continuation_keeps_indent () =
  let width = 40 in
  let model =
    with_cursor_internal
      (with_lines (initial_model width) [ us "{"; us "  foo <- 1"; us "}" ])
      ~line:1 ~pos:10
  in
  match update (Update.Key Tty_listener.Enter) model with
  | Continue new_model ->
      Alcotest.(check (list string))
        "lines after enter"
        [ "{"; "  foo <- 1"; "  "; "}" ]
        (to_strings new_model.lines);
      Alcotest.(check int) "cursor line" 2 new_model.cursor_line;
      Alcotest.(check int) "cursor pos" 2 new_model.cursor_pos
  | _ -> Alcotest.fail "Expected Continue"

(* Completion tests *)

let test_completion_dropdown_size_max_4 () =
  let cs = Completion.create ~token_start:0 (make_completion_items 10) in
  Alcotest.(check int) "dropdown size capped at 4" 4 (Completion.dropdown_size cs)

let test_completion_visible_items_without_selection () =
  let cs = Completion.create ~token_start:0 (make_completion_items 6) in
  Alcotest.(check (list string))
    "shows first four items before cycling"
    [ "item0"; "item1"; "item2"; "item3" ]
    (Completion.visible_items cs)

let test_completion_visible_items_short_list () =
  let cs = Completion.create ~token_start:0 [ "item0"; "item1"; "item2" ] in
  Alcotest.(check (list string))
    "shows all items when fewer than four"
    [ "item0"; "item1"; "item2" ]
    (Completion.visible_items cs)

let test_completion_visible_window_stays_near_top () =
  let cs =
    Completion.create ~token_start:0 (make_completion_items 10)
    |> fun cs -> cycle_completion_many cs 3
  in
  Alcotest.(check int) "window start stays at zero" 0
    (Completion.visible_window_start cs);
  Alcotest.(check (list string))
    "selected index 2 still shows first page"
    [ "item0"; "item1"; "item2"; "item3" ]
    (Completion.visible_items cs);
  Alcotest.(check (option int))
    "selected row is index 2 in window"
    (Some 2) (Completion.selected_index_in_window cs)

let test_completion_visible_window_scrolls () =
  let cs =
    Completion.create ~token_start:0 (make_completion_items 10)
    |> fun cs -> cycle_completion_many cs 4
  in
  Alcotest.(check int) "window start shifts once selection reaches row 3" 1
    (Completion.visible_window_start cs);
  Alcotest.(check (list string))
    "window scrolls forward"
    [ "item1"; "item2"; "item3"; "item4" ]
    (Completion.visible_items cs);
  Alcotest.(check (option int))
    "selected stays on third visible row when possible"
    (Some 2) (Completion.selected_index_in_window cs)

let test_completion_visible_window_clamps_to_end () =
  let cs =
    Completion.create ~token_start:0 (make_completion_items 10)
    |> fun cs -> cycle_completion_many cs 10
  in
  Alcotest.(check int) "window clamps to last four items" 6
    (Completion.visible_window_start cs);
  Alcotest.(check (list string))
    "shows final window"
    [ "item6"; "item7"; "item8"; "item9" ]
    (Completion.visible_items cs);
  Alcotest.(check (option int))
    "final selection can sit on bottom row"
    (Some 3) (Completion.selected_index_in_window cs)

let test_completion_visible_window_wraps_to_top () =
  let cs =
    Completion.create ~token_start:0 (make_completion_items 10)
    |> fun cs -> cycle_completion_many cs 11
  in
  Alcotest.(check int) "window resets after wraparound" 0
    (Completion.visible_window_start cs);
  Alcotest.(check (list string))
    "shows first page again after wrapping"
    [ "item0"; "item1"; "item2"; "item3" ]
    (Completion.visible_items cs);
  Alcotest.(check (option int))
    "wrapped selection returns to first row"
    (Some 0) (Completion.selected_index_in_window cs)

let test_view_completion_rows_capped_at_4 () =
  let completion = Completion.create ~token_start:0 (make_completion_items 10) in
  let model =
    { (with_lines (initial_model 20) [ us "item" ]) with completion = Some completion }
    |> fun model -> with_cursor_internal model ~line:0 ~pos:4
  in
  let rows = completion_rows model in
  Alcotest.(check int) "renders four completion rows" 4 (List.length rows);
  Alcotest.(check (list string))
    "renders first four items"
    [ "item0"; "item1"; "item2"; "item3" ]
    (List.map completion_row_text rows)

let test_view_completion_rows_scroll_with_tab () =
  let base_model =
    with_lines (initial_model 20) [ us "it" ]
    |> fun model -> with_cursor_internal model ~line:0 ~pos:2
  in
  let model =
    match
      update
        (Update.Response (Ffi_backend.Completions ("it", make_completion_items 10)))
        base_model
    with
    | Continue model -> model
    | _ -> Alcotest.fail "Expected completion response to continue"
  in
  let model =
    List.fold_left
      (fun model _ ->
        match update (Update.Key Tty_listener.Tab) model with
        | Continue model -> model
        | _ -> Alcotest.fail "Expected tab to keep cycling completions")
      model [ (); (); (); () ]
  in
  Alcotest.(check string) "tab inserts selected completion" "item3"
    (first_line_str model);
  let rows = completion_rows model in
  Alcotest.(check int) "still renders four rows after scrolling" 4
    (List.length rows);
  Alcotest.(check (list string))
    "viewport scrolls to keep selection visible"
    [ "item1"; "item2"; "item3"; "item4" ]
    (List.map completion_row_text rows);
  Alcotest.(check (list bool))
    "selected row is the third visible row"
    [ false; false; true; false ]
    (List.map completion_row_selected rows)

(* Prompt placement tests *)

let test_clamp_prompt_top_mid_screen () =
  (* Cursor at row 5 in a 25-row terminal: stays at 5 *)
  let result = Frontend_types.clamp_prompt_top 25 5 in
  Alcotest.(check int) "stays at row 5" 5 result

let test_clamp_prompt_top_at_bottom_zone () =
  (* Row 21 = term_height - 4 = exactly at the cap *)
  let result = Frontend_types.clamp_prompt_top 25 21 in
  Alcotest.(check int) "at bottom zone" 21 result

let test_clamp_prompt_top_past_bottom_zone () =
  (* Row 24 is past the cap, gets clamped to 21 *)
  let result = Frontend_types.clamp_prompt_top 25 24 in
  Alcotest.(check int) "clamped to bottom zone" 21 result

let test_clamp_prompt_top_at_top () =
  (* Row 1 is above the minimum, clamped to 2 *)
  let result = Frontend_types.clamp_prompt_top 25 1 in
  Alcotest.(check int) "clamped to row 2" 2 result

let test_clamp_prompt_top_small_terminal () =
  (* term_height=6: default_prompt_top = max(2, 6-4) = 2, so clamp to 2 *)
  let result = Frontend_types.clamp_prompt_top 6 10 in
  Alcotest.(check int) "small terminal clamp" 2 result

let test_prompt_only_moves_down () =
  (* Prompt at row 8, after short output ending at row 6: prompt stays at 8 *)
  let width = 20 in
  let model =
    {
      (initial_model width) with
      prompt_top_row = 8;
      term_height = 25;
      prompt_box_height = Frontend_types.min_prompt_height;
    }
  in
  (* Simulate Done: next_prompt_row = 7 (below output at row 6) *)
  let next_prompt_row = 7 in
  let natural = max model.prompt_top_row next_prompt_row in
  let clamped = Frontend_types.clamp_prompt_top model.term_height natural in
  Alcotest.(check int) "prompt stays at 8" 8 clamped

let test_prompt_moves_down_with_output () =
  (* Prompt at row 5, output pushes next prompt to row 10 *)
  let width = 20 in
  let model =
    { (initial_model width) with prompt_top_row = 5; term_height = 25 }
  in
  let next_prompt_row = 10 in
  let natural = max model.prompt_top_row next_prompt_row in
  let clamped = Frontend_types.clamp_prompt_top model.term_height natural in
  Alcotest.(check int) "prompt moves to 10" 10 clamped

let test_prompt_capped_at_bottom_zone () =
  (* Prompt at row 15, output pushes to row 24, capped at 21 *)
  let width = 20 in
  let model =
    { (initial_model width) with prompt_top_row = 15; term_height = 25 }
  in
  let next_prompt_row = 24 in
  let natural = max model.prompt_top_row next_prompt_row in
  let clamped = Frontend_types.clamp_prompt_top model.term_height natural in
  Alcotest.(check int) "capped at 21" 21 clamped

let test_submit_prompt_box_height () =
  (* After submit, prompt_box_height should be min_prompt_height *)
  let width = 20 in
  let model =
    { (initial_model width) with prompt_top_row = 5; term_height = 25 }
  in
  let model = insert_many model 3 in
  match submit model with
  | Submit (_, new_model) ->
      Alcotest.(check int)
        "prompt_box_height is min_prompt_height"
        Frontend_types.min_prompt_height new_model.prompt_box_height
  | _ -> Alcotest.fail "Expected Submit"

let test_resize_clamps_prompt () =
  (* Prompt at row 15, terminal shrinks from 25 to 12 *)
  let width = 20 in
  let model =
    { (initial_model width) with prompt_top_row = 15; term_height = 25 }
  in
  let new_model = Update.handle_resize 20 12 model in
  (* default_prompt_top 12 = max(2, 12-4) = 8. clamp(12, 15) = min(15, 8) = 8 *)
  Alcotest.(check int) "prompt clamped on shrink" 8 new_model.prompt_top_row

let test_resize_preserves_prompt_in_zone () =
  (* Prompt at row 5, terminal stays 25: no change *)
  let width = 20 in
  let model =
    { (initial_model width) with prompt_top_row = 5; term_height = 25 }
  in
  let new_model = Update.handle_resize 20 25 model in
  (* No change: 5 <= 21, so stays at 5 *)
  Alcotest.(check int) "prompt stays at 5" 5 new_model.prompt_top_row

let test_min_prompt_height_enforced () =
  (* handle_vertical_cursor_movement enforces min_prompt_height *)
  let width = 20 in
  let model =
    {
      (initial_model width) with
      prompt_top_row = 5;
      term_height = 25;
      prompt_box_height = 1;
    }
  in
  let new_model = Update.handle_vertical_cursor_movement model in
  Alcotest.(check int)
    "prompt_box_height at least min" Frontend_types.min_prompt_height
    new_model.prompt_box_height

(* Readline mode tests *)

let readline_model width prompt =
  {
    (initial_model width) with
    mode = Frontend_types.Readline prompt;
    awaiting_response = true;
  }

let test_readline_response_sets_mode () =
  let width = 20 in
  let model =
    {
      (initial_model width) with
      backend_response = Some (Ffi_backend.Readline "Enter name: ");
      awaiting_response = true;
    }
  in
  let new_model = Update.process_response model in
  Alcotest.(check bool)
    "mode is Readline" true
    (new_model.mode = Frontend_types.Readline "Enter name: ");
  Alcotest.(check bool)
    "awaiting_response stays true" true new_model.awaiting_response

let test_readline_empty_prompt_normalized () =
  let width = 20 in
  let model =
    {
      (initial_model width) with
      backend_response = Some (Ffi_backend.Readline "");
      awaiting_response = true;
    }
  in
  let new_model = Update.process_response model in
  Alcotest.(check bool)
    "empty prompt normalized to 'input'" true
    (new_model.mode = Frontend_types.Readline "input")

let test_readline_done_resets_mode () =
  let width = 20 in
  let model =
    {
      (readline_model width "Enter name: ") with
      backend_response = Some Ffi_backend.Done;
    }
  in
  let new_model = Update.process_response model in
  Alcotest.(check bool)
    "mode reset to Normal" true
    (new_model.mode = Frontend_types.Normal)

let test_readline_typing_works () =
  let width = 20 in
  let model = readline_model width "Enter name: " in
  match update (Update.Key (Tty_listener.Char "A")) model with
  | Continue new_model ->
      Alcotest.(check string) "char inserted" "A" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_readline_up_blocked () =
  let width = 20 in
  let model = with_lines (readline_model width "Enter: ") [ us "hello" ] in
  match update (Update.Key Tty_listener.Up) model with
  | Continue new_model ->
      Alcotest.(check string)
        "line unchanged" "hello" (first_line_str new_model)
  | _ -> Alcotest.fail "Expected Continue"

let test_readline_down_blocked () =
  let width = 20 in
  let model = with_lines (readline_model width "Enter: ") [ us "hello" ] in
  match update (Update.Key Tty_listener.Down) model with
  | Continue new_model ->
      Alcotest.(check string)
        "line unchanged" "hello" (first_line_str new_model)
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
          test_case "Wrapped syntax highlighting" `Quick
            test_wrapped_syntax_highlighting;
          test_case "Exact width wrapped row stays aligned" `Quick
            test_wrapped_exact_width_keeps_trailing_row;
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
          test_case "Wide grapheme movement" `Quick
            test_cursor_moves_over_wide_grapheme;
          test_case "Emoji ZWJ movement" `Quick test_cursor_moves_over_emoji_zwj;
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
          test_case "Continuation if paren" `Quick
            test_submit_continuation_if_paren;
          test_case "Continuation function paren" `Quick
            test_submit_continuation_function_paren;
          test_case "If braced single line" `Quick
            test_submit_if_braced_single_line;
          test_case "Lambda body same line" `Quick
            test_submit_lambda_body_same_line;
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
          test_case "View clears reserved output row" `Quick
            test_view_clears_reserved_output_row_before_first_chunk;
          test_case "View preserves streamed output row" `Quick
            test_view_preserves_streamed_output_row;
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
      ( "lex_cache",
        [
          test_case "Single line edit" `Quick test_lex_cache_single_line_edit;
          test_case "Multiline mode change" `Quick
            test_lex_cache_multiline_mode_change;
          test_case "Delete empty line" `Quick test_lex_delete_empty_line;
        ] );
      ( "completion",
        [
          test_case "Dropdown size capped at 4" `Quick
            test_completion_dropdown_size_max_4;
          test_case "Visible items without selection" `Quick
            test_completion_visible_items_without_selection;
          test_case "Visible items short list" `Quick
            test_completion_visible_items_short_list;
          test_case "Visible window stays near top" `Quick
            test_completion_visible_window_stays_near_top;
          test_case "Visible window scrolls" `Quick
            test_completion_visible_window_scrolls;
          test_case "Visible window clamps to end" `Quick
            test_completion_visible_window_clamps_to_end;
          test_case "Visible window wraps to top" `Quick
            test_completion_visible_window_wraps_to_top;
          test_case "View caps completion rows at 4" `Quick
            test_view_completion_rows_capped_at_4;
          test_case "View scrolls completion rows with tab" `Quick
            test_view_completion_rows_scroll_with_tab;
        ] );
      ( "prompt_placement",
        [
          test_case "Clamp mid screen" `Quick test_clamp_prompt_top_mid_screen;
          test_case "Clamp at bottom zone" `Quick
            test_clamp_prompt_top_at_bottom_zone;
          test_case "Clamp past bottom zone" `Quick
            test_clamp_prompt_top_past_bottom_zone;
          test_case "Clamp at top" `Quick test_clamp_prompt_top_at_top;
          test_case "Clamp small terminal" `Quick
            test_clamp_prompt_top_small_terminal;
          test_case "Prompt only moves down" `Quick test_prompt_only_moves_down;
          test_case "Prompt moves down with output" `Quick
            test_prompt_moves_down_with_output;
          test_case "Prompt capped at bottom zone" `Quick
            test_prompt_capped_at_bottom_zone;
          test_case "Submit sets prompt_box_height" `Quick
            test_submit_prompt_box_height;
          test_case "Resize clamps prompt" `Quick test_resize_clamps_prompt;
          test_case "Resize preserves prompt in zone" `Quick
            test_resize_preserves_prompt_in_zone;
          test_case "Min prompt height enforced" `Quick
            test_min_prompt_height_enforced;
        ] );
      ( "readline_mode",
        [
          test_case "Readline response sets mode" `Quick
            test_readline_response_sets_mode;
          test_case "Empty prompt normalized" `Quick
            test_readline_empty_prompt_normalized;
          test_case "Done resets mode" `Quick test_readline_done_resets_mode;
          test_case "Typing works in readline mode" `Quick
            test_readline_typing_works;
          test_case "Up blocked in readline mode" `Quick
            test_readline_up_blocked;
          test_case "Down blocked in readline mode" `Quick
            test_readline_down_blocked;
        ] );
      ( "matched_brackets",
        [
          test_case "Insert matched paren" `Quick test_insert_matched_paren;
          test_case "Insert matched bracket" `Quick test_insert_matched_bracket;
          test_case "Insert matched brace" `Quick test_insert_matched_brace;
          test_case "Insert matched quote" `Quick test_insert_matched_quote;
          test_case "Skip closing paren" `Quick test_skip_closing_paren;
          test_case "Skip closing quote" `Quick test_skip_closing_quote;
          test_case "Delete matched paren" `Quick test_delete_matched_paren;
          test_case "Delete matched bracket" `Quick test_delete_matched_bracket;
          test_case "Delete matched brace" `Quick test_delete_matched_brace;
          test_case "Delete matched quote" `Quick test_delete_matched_quote;
          test_case "Delete unmatched paren" `Quick test_delete_unmatched_paren;
          test_case "Matched insert with content" `Quick
            test_matched_insert_with_content;
          test_case "Empty brace expands on enter" `Quick
            test_empty_brace_expands_on_enter;
          test_case "Brace continuation keeps indent" `Quick
            test_brace_continuation_keeps_indent;
        ] );
    ]
