open Raoui

let us s = Unicode_string.of_string s |> Result.get_ok

let with_temp_history f =
  let path = Filename.temp_file "raoui-history-" ".sqlite" in
  Sys.remove path;
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)

let close history = History.close history

let test_history_persists_navigation_order () =
  with_temp_history @@ fun path ->
  let history = History.init path in
  History.add_to_history history [ us "first" ];
  History.add_to_history history [ us "second" ];
  Alcotest.(check (array string))
    "newest first" [| "second"; "first" |] (History.get_all history);
  close history;
  let history = History.init path in
  Alcotest.(check (array string))
    "loaded newest first" [| "second"; "first" |] (History.get_all history);
  close history

let test_history_search_matches_existing_behavior () =
  with_temp_history @@ fun path ->
  let history = History.init path in
  History.add_to_history history [ us "alpha <- 1" ];
  History.add_to_history history [ us "beta <- alpha + 1" ];
  Alcotest.(check (list (pair string string)))
    "case insensitive substring"
    [ ("r", "beta <- alpha + 1") ]
    (History.search_matches history "ALPHA +" ~limit:10);
  Alcotest.(check (list (pair string string)))
    "empty needle matches everything, newest first"
    [ ("r", "beta <- alpha + 1"); ("r", "alpha <- 1") ]
    (History.search_matches history "" ~limit:10);
  Alcotest.(check (list (pair string string)))
    "limit caps results"
    [ ("r", "beta <- alpha + 1") ]
    (History.search_matches history "" ~limit:1);
  close history

let test_recent_interactions_include_outputs () =
  with_temp_history @@ fun path ->
  let history = History.init path in
  History.add_to_history history [ us "1 + 1" ];
  History.record_response history (Ffi_backend.Stdout "calculating\n");
  History.record_response history (Ffi_backend.Result "[1] 2\n");
  History.record_response history Ffi_backend.Done;
  let interaction =
    match History.recent_interactions history ~limit:1 with
    | [ interaction ] -> interaction
    | _ -> Alcotest.fail "expected one recent interaction"
  in
  Alcotest.(check string) "input" "1 + 1" interaction.input;
  Alcotest.(check string) "mode" "r" interaction.mode;
  Alcotest.(check int) "output count" 2 (List.length interaction.outputs);
  let output_texts =
    List.map
      (fun output -> (output.History.kind, output.History.text))
      interaction.outputs
  in
  Alcotest.(check (list (pair string (option string))))
    "outputs"
    [ ("stdout", Some "calculating\n"); ("result", Some "[1] 2\n") ]
    output_texts;
  close history

let test_search_interactions_matches_input_and_output () =
  with_temp_history @@ fun path ->
  let history = History.init path in
  (* A command whose input mentions the keyword. *)
  History.add_to_history history [ us "summary(widgets)" ];
  History.record_response history Ffi_backend.Done;
  (* A command whose input does NOT mention it but whose output does. *)
  History.add_to_history history [ us "names(df)" ];
  History.record_response history (Ffi_backend.Result "[1] \"widgets\"\n");
  History.record_response history Ffi_backend.Done;
  (* A command that mentions neither. *)
  History.add_to_history history [ us "rnorm(10)" ];
  History.record_response history Ffi_backend.Done;
  let inputs =
    History.search_interactions history ~keyword:"widgets" ~limit:20
    |> List.map (fun it -> it.History.input)
  in
  (* Raw result is oldest-first, like recent_interactions; the MCP layer
     reverses for display. *)
  Alcotest.(check (list string))
    "matches input and output, excludes non-matches"
    [ "summary(widgets)"; "names(df)" ]
    inputs;
  Alcotest.(check int)
    "no match yields empty" 0
    (List.length (History.search_interactions history ~keyword:"zzz" ~limit:20));
  close history

let test_search_interactions_escapes_like_wildcards () =
  with_temp_history @@ fun path ->
  let history = History.init path in
  (* A command with a literal underscore, and one where any char sits in that
     position. A LIKE wildcard '_' would match both; an escaped one only a_b. *)
  History.add_to_history history [ us "a_b <- 1" ];
  History.add_to_history history [ us "axb <- 2" ];
  let inputs keyword =
    History.search_interactions history ~keyword ~limit:20
    |> List.map (fun it -> it.History.input)
  in
  Alcotest.(check (list string))
    "underscore is literal, not a wildcard" [ "a_b <- 1" ] (inputs "a_b");
  Alcotest.(check (list string))
    "percent is literal, not a wildcard" [] (inputs "a%b");
  close history

let test_empty_submission_is_not_recorded () =
  with_temp_history @@ fun path ->
  let history = History.init path in
  History.add_to_history history [ us "real" ];
  History.add_to_history history [ us "" ];
  History.add_to_history history [ us "   " ];
  History.add_to_history history [ us ""; us "  " ];
  Alcotest.(check (array string))
    "blank submissions skipped" [| "real" |] (History.get_all history);
  close history

let () =
  Alcotest.run "history"
    [
      ( "sqlite",
        [
          Alcotest.test_case "persists navigation order" `Quick
            test_history_persists_navigation_order;
          Alcotest.test_case "empty submission is not recorded" `Quick
            test_empty_submission_is_not_recorded;
          Alcotest.test_case "search matches existing behavior" `Quick
            test_history_search_matches_existing_behavior;
          Alcotest.test_case "recent interactions include outputs" `Quick
            test_recent_interactions_include_outputs;
          Alcotest.test_case "search interactions match input and output" `Quick
            test_search_interactions_matches_input_and_output;
          Alcotest.test_case "search interactions escape LIKE wildcards" `Quick
            test_search_interactions_escapes_like_wildcards;
        ] );
    ]
