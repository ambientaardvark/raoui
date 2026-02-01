open Raoui

(* Integration tests that start the ark kernel *)

let find_project_root () =
  let candidates =
    [
      ".";                (* Running from project root *)
      "../../..";         (* Running from _build/default/test *)
    ]
  in
  List.find_opt (fun dir ->
      Sys.file_exists (Filename.concat dir "dune-project"))
    candidates

let wait_until_ready backend =
  let rec loop () =
    if Backend.poll_ready backend then ()
    else (
      Eio.Fiber.yield ();
      loop ())
  in
  loop ()

let with_backend f =
  match find_project_root () with
  | None -> failwith "project root not found (no dune-project)"
  | Some project_root ->
      let orig_dir = Sys.getcwd () in
      Sys.chdir project_root;
      Fun.protect
        (fun () ->
          Eio_main.run @@ fun _env ->
          let backend = Backend.create () in
          Fun.protect
            (fun () -> f backend)
            ~finally:(fun () -> Backend.deinit backend))
        ~finally:(fun () -> Sys.chdir orig_dir)

let test_kernel_starts () =
  with_backend (fun backend ->
      (* Poll with a timeout to avoid hanging forever *)
      let start = Unix.gettimeofday () in
      let timeout = 30.0 in
      let rec loop () =
        if Backend.poll_ready backend then ()
        else if Unix.gettimeofday () -. start > timeout then
          Alcotest.fail "Timed out waiting for kernel to become ready"
        else (
          Unix.sleepf 0.1;
          loop ())
      in
      loop ())

let test_simple_expression () =
  with_backend (fun backend ->
      wait_until_ready backend;
      Backend.submit backend "1 + 1";
      let rec collect_responses acc =
        match Backend.await_response backend with
        | Backend.Done -> List.rev acc
        | Backend.Shutdown -> failwith "Unexpected shutdown"
        | chunk -> collect_responses (chunk :: acc)
      in
      let responses = collect_responses [] in
      (* Should have a Result with "2" or "[1] 2" *)
      let has_result =
        List.exists
          (function Backend.Result s -> String.length s > 0 | _ -> false)
          responses
      in
      Alcotest.(check bool) "got result" true has_result)

let test_submit_before_ready () =
  with_backend (fun backend ->
      (* Submit immediately, before calling poll_ready *)
      Backend.submit backend "2 + 2";
      (* Now wait for ready - this should flush the pending submit *)
      wait_until_ready backend;
      let rec collect_responses acc =
        match Backend.await_response backend with
        | Backend.Done -> List.rev acc
        | Backend.Shutdown -> failwith "Unexpected shutdown"
        | chunk -> collect_responses (chunk :: acc)
      in
      let responses = collect_responses [] in
      let has_result =
        List.exists
          (function Backend.Result s -> String.length s > 0 | _ -> false)
          responses
      in
      Alcotest.(check bool) "got result after queued submit" true has_result)

let test_poll_ready_returns_true_when_ready () =
  with_backend (fun backend ->
      wait_until_ready backend;
      (* After wait_until_ready, poll_ready should return true immediately *)
      let ready = Backend.poll_ready backend in
      Alcotest.(check bool) "poll_ready returns true" true ready)

let () =
  let open Alcotest in
  run "Ark Integration"
    [
      ( "kernel",
        [
          test_case "Kernel starts and becomes ready" `Slow test_kernel_starts;
          test_case "Simple expression" `Slow test_simple_expression;
          test_case "Submit before ready" `Slow test_submit_before_ready;
          test_case "poll_ready returns true when ready" `Slow
            test_poll_ready_returns_true_when_ready;
        ] );
    ]
