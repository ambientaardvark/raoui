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
  Alcotest.(check string)
    "case insensitive substring" "beta <- alpha + 1"
    (History.search_history history "%ALPHA +%");
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

let () =
  Alcotest.run "history"
    [
      ( "sqlite",
        [
          Alcotest.test_case "persists navigation order" `Quick
            test_history_persists_navigation_order;
          Alcotest.test_case "search matches existing behavior" `Quick
            test_history_search_matches_existing_behavior;
          Alcotest.test_case "recent interactions include outputs" `Quick
            test_recent_interactions_include_outputs;
        ] );
    ]
