open Raoui

let row entry_index line first hidden_lines =
  { Search_pager.entry_index; line; first; hidden_lines }

let pp_row fmt (r : Search_pager.row) =
  Format.fprintf fmt "{entry=%d; line=%S; first=%b; hidden=%d}" r.entry_index
    r.line r.first r.hidden_lines

let row_t = Alcotest.testable pp_row ( = )

let matches_of_texts texts = List.map (fun t -> ("r", t)) texts

let test_empty () =
  Alcotest.(check (list row_t))
    "no matches, no rows" []
    (Search_pager.view_rows ~max_rows:10 ~selected:0 []);
  Alcotest.(check (list row_t))
    "no budget, no rows" []
    (Search_pager.view_rows ~max_rows:0 ~selected:0
       (matches_of_texts [ "a" ]))

let test_single_line_entries () =
  let matches = matches_of_texts [ "a"; "b"; "c" ] in
  Alcotest.(check (list row_t))
    "one row per entry"
    [ row 0 "a" true 0; row 1 "b" true 0; row 2 "c" true 0 ]
    (Search_pager.view_rows ~max_rows:10 ~selected:0 matches)

let test_more_matches_than_rows () =
  let matches = matches_of_texts [ "a"; "b"; "c"; "d"; "e" ] in
  Alcotest.(check (list row_t))
    "budget caps visible entries"
    [ row 0 "a" true 0; row 1 "b" true 0; row 2 "c" true 0 ]
    (Search_pager.view_rows ~max_rows:3 ~selected:0 matches)

let test_window_follows_selection () =
  let matches = matches_of_texts [ "a"; "b"; "c"; "d"; "e" ] in
  Alcotest.(check (list row_t))
    "window slides so the selected entry is visible"
    [ row 2 "c" true 0; row 3 "d" true 0; row 4 "e" true 0 ]
    (Search_pager.view_rows ~max_rows:3 ~selected:4 matches)

let test_selected_multiline_expands () =
  let matches = matches_of_texts [ "f <- function(x) {\n  x + 1\n}"; "b" ] in
  Alcotest.(check (list row_t))
    "selected entry shows all its lines"
    [
      row 0 "f <- function(x) {" true 0;
      row 0 "  x + 1" false 0;
      row 0 "}" false 0;
      row 1 "b" true 0;
    ]
    (Search_pager.view_rows ~max_rows:10 ~selected:0 matches)

let test_unselected_multiline_collapses () =
  let matches = matches_of_texts [ "f <- function(x) {\n  x + 1\n}"; "b" ] in
  Alcotest.(check (list row_t))
    "unselected entry collapses to first line with hidden count"
    [ row 0 "f <- function(x) {" true 2; row 1 "b" true 0 ]
    (Search_pager.view_rows ~max_rows:10 ~selected:1 matches)

let test_expansion_leaves_context () =
  (* 10 rows, selected entry has 12 lines, 4 other matches: expansion caps at
     10 - 3 = 7 rows so 3 alternatives stay visible. *)
  let big = String.concat "\n" (List.init 12 string_of_int) in
  let matches = matches_of_texts [ big; "b"; "c"; "d"; "e" ] in
  let rows = Search_pager.view_rows ~max_rows:10 ~selected:0 matches in
  let selected_rows =
    List.filter (fun (r : Search_pager.row) -> r.entry_index = 0) rows
  in
  Alcotest.(check int) "selected expands to 7 rows" 7
    (List.length selected_rows);
  Alcotest.(check int)
    "hidden count on last selected row" 5
    (List.nth selected_rows 6).Search_pager.hidden_lines;
  Alcotest.(check int) "10 rows total" 10 (List.length rows);
  Alcotest.(check int)
    "3 context entries visible" 3
    (List.length
       (List.filter (fun (r : Search_pager.row) -> r.entry_index > 0) rows))

let test_deep_selection_with_expansion () =
  (* Selected multiline entry at the end of a long list: context reservation
     caps its expansion at 5 - 3 = 2 rows, and the window slides so that
     capped block fits. *)
  let matches =
    matches_of_texts [ "a"; "b"; "c"; "d"; "e1\ne2\ne3" ]
  in
  Alcotest.(check (list row_t))
    "selected block visible at window end, capped by context"
    [
      row 1 "b" true 0;
      row 2 "c" true 0;
      row 3 "d" true 0;
      row 4 "e1" true 0;
      row 4 "e2" false 1;
    ]
    (Search_pager.view_rows ~max_rows:5 ~selected:4 matches)

let () =
  let open Alcotest in
  run "Search_pager"
    [
      ( "view_rows",
        [
          test_case "Empty" `Quick test_empty;
          test_case "Single-line entries" `Quick test_single_line_entries;
          test_case "Budget caps entries" `Quick test_more_matches_than_rows;
          test_case "Window follows selection" `Quick
            test_window_follows_selection;
          test_case "Selected multiline expands" `Quick
            test_selected_multiline_expands;
          test_case "Unselected multiline collapses" `Quick
            test_unselected_multiline_collapses;
          test_case "Expansion leaves context" `Quick
            test_expansion_leaves_context;
          test_case "Deep selection with expansion" `Quick
            test_deep_selection_with_expansion;
        ] );
    ]
