type image = {
  source_path : string;
  preview_path : string;
  mime_type : string option;
  width_px : int option;
  height_px : int option;
}

type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown
  | Restarted of string
  | Image of image
  | Passthrough
  | Passthrough_end
  | Completions of string * string list  (* token * items *)
  | Readline of string  (* prompt *)
  | Ai_output of string  (* a chunk of AI assistant text *)
  | Ai_tool_call of string  (* the AI invoked a tool; carries its short name *)
  | Ai_suggestion of string  (* R code the AI suggests dropping into the prompt *)
  | Ai_done  (* AI response stream complete *)

type completion = string


type t = {
  sleep : float -> unit;
  mutable ready : bool;
  mutable busy : bool;
  mutable suppressing : bool;
  pending : [ `User of string | `Background of string ] Queue.t;
  mutable stashed : (int * int * string) option;
  (* Console output captured during R init (.Rprofile etc.), awaiting delivery to
     the frontend as a single Stdout chunk above the first prompt. None once
     drained. *)
  mutable pending_startup_output : string option;
}

let log_snippet s =
  let compact =
    s |> String.split_on_char '\n' |> String.concat "\\n" |> String.trim
  in
  if String.length compact > 200 then String.sub compact 0 200 ^ "..."
  else compact

let r_home () =
  let has_libr home =
    let libdir = Filename.concat home "lib" in
    Stdlib.Sys.file_exists (Filename.concat libdir "libR.dylib")
    || Stdlib.Sys.file_exists (Filename.concat libdir "libR.so")
  in
  let discover_with_r () =
    try
      let ic = Unix.open_process_in "R RHOME 2>/dev/null" in
      let line =
        try Some (input_line ic) with End_of_file -> None
      in
      let status = Unix.close_process_in ic in
      match status, line with
      | Unix.WEXITED 0, Some h when String.length h > 0 -> Some h
      | _ -> None
    with _ -> None
  in
  match Stdlib.Sys.getenv_opt "R_HOME" with
  | Some h when has_libr h -> h
  | Some _ -> failwith "R_HOME is set but does not contain lib/libR"
  | None ->
    (match discover_with_r () with
    | Some h when has_libr h -> h
    | _ ->
      let candidates =
        [
          "/Library/Frameworks/R.framework/Resources";
          "/usr/lib/R";
          "/usr/lib64/R";
          "/usr/local/lib/R";
          "/usr/local/lib64/R";
        ]
      in
      match List.find_opt has_libr candidates with
      | Some h -> h
      | None -> failwith "R_HOME not set and could not discover libR")

let start_eval t code =
  t.busy <- true;
  Logs.debug (fun m -> m "starting R eval: %s" (log_snippet code));
  Rffi.submit code

let flush_pending t =
  match Queue.pop t.pending with
  | `User code ->
    t.suppressing <- false;
    Logs.debug (fun m -> m "flushing queued user eval");
    start_eval t code
  | `Background code ->
    t.suppressing <- true;
    Logs.debug (fun m -> m "flushing queued background eval");
    start_eval t code
  | exception Queue.Empty -> ()

let create ~sw ~clock () =
  let home = r_home () in
  Logs.info (fun m -> m "using R home %s" home);
  let sleep = Eio.Time.sleep clock in
  let t = { sleep; ready = false; busy = false; suppressing = false;
            pending = Queue.create (); stashed = None;
            pending_startup_output = None } in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rc = Eio_unix.run_in_systhread (fun () -> Rffi.init home) in
    if rc <> 0 then failwith "Failed to initialize R runtime";
    t.ready <- true;
    (match Rffi.take_init_output () with
     | "" -> ()
     | output -> t.pending_startup_output <- Some output);
    Logs.info (fun m -> m "R runtime initialized");
    flush_pending t;
    `Stop_daemon);
  t

let poll_ready t = t.ready

let submit t code =
  t.suppressing <- false;
  if t.ready && not t.busy then start_eval t code
  else begin
    Logs.debug (fun m -> m "queueing user eval");
    Queue.push (`User code) t.pending
  end

let background_submit t code =
  if t.ready && not t.busy then begin
    t.suppressing <- true;
    Logs.info (fun m -> m "starting background eval: %s" (log_snippet code));
    start_eval t code
  end else begin
    Logs.debug (fun m -> m "queueing background eval");
    Queue.push (`Background code) t.pending
  end

let parse_image_payload payload =
  let split_first_eq line =
    match String.index_opt line '=' with
    | None -> None
    | Some i ->
        let key = String.trim (String.sub line 0 i) in
        let value = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
        Some (key, value)
  in
  let from_pairs lines =
    List.filter_map split_first_eq lines
  in
  let lines = String.split_on_char '\n' payload |> List.filter (fun s -> s <> "") in
  match lines with
  | [] -> None
  | [ path ] ->
      Some
        {
          source_path = path;
          preview_path = path;
          mime_type = None;
          width_px = None;
          height_px = None;
        }
  | _ ->
      let pairs = from_pairs lines in
      let source_path =
        match List.assoc_opt "source_path" pairs with
        | Some path -> Some path
        | None -> List.assoc_opt "path" pairs
      in
      let preview_path =
        match List.assoc_opt "preview_path" pairs with
        | Some path -> Some path
        | None -> List.assoc_opt "path" pairs
      in
      Option.bind source_path (fun source_path ->
        Option.map
          (fun preview_path ->
          {
            source_path;
            preview_path;
            mime_type = List.assoc_opt "mime" pairs;
            width_px = Option.bind (List.assoc_opt "width" pairs) int_of_string_opt;
            height_px = Option.bind (List.assoc_opt "height" pairs) int_of_string_opt;
          })
          preview_path)

let map_kind kind payload =
  match kind with
  | 0 (* STDOUT *)         -> Stdout payload
  | 1 (* STDERR *)         -> Stdout payload
  | 2 (* RESULT *)         -> Result payload
  | 3 (* R_ERROR *)        -> R_error payload
  | 4 (* INTERNAL_ERROR *) -> Internal_error payload
  | 5 (* DONE *)           -> Done
  | 6 (* IMAGE *)          -> (
      match parse_image_payload payload with
      | Some image -> Image image
      | None -> Internal_error "Invalid image payload")
  | 8 (* PASSTHROUGH *)    -> Passthrough
  | 9 (* PASSTHROUGH_END *)-> Passthrough_end
  | 10 (* COMPLETIONS *)   ->
    if String.length payload = 0 then Completions ("", [])
    else
      (match String.split_on_char '\n' payload with
       | token :: items -> Completions (token, items)
       | [] -> Completions ("", []))
  | 11 (* READLINE *)      -> Readline payload
  | _ -> Internal_error (Printf.sprintf "Unknown message kind: %d" kind)

let await_response t =
  let pop () =
    match t.stashed with
    | Some msg -> t.stashed <- None; Some msg
    | None -> Rffi.rb_pop ()
  in
  let handle_done () =
    t.busy <- false;
    t.suppressing <- false;
    flush_pending t
  in
  let rec drain buf =
    match Rffi.rb_pop () with
    | Some (k, _, p) when k = 0 || k = 1 ->
      Buffer.add_string buf p;
      drain buf
    | other ->
      t.stashed <- other;
      Stdout (Buffer.contents buf)
  in
  let rec loop () =
    match t.pending_startup_output with
    | Some output ->
      (* Deliver init output (.Rprofile etc.) once, before anything from the ring
         buffer, so it renders above the first prompt. *)
      t.pending_startup_output <- None;
      Stdout output
    | None ->
    match pop () with
    | None -> t.sleep 0.01; loop ()
    | Some (kind, _flags, payload) ->
      if t.suppressing && kind <> 5 && kind <> 8 && kind <> 9 && kind <> 10
         && kind <> 11 && kind <> 12 then begin
        (match kind with
         | 3 -> Logs.err (fun m -> m "suppressed background R error: %s" (log_snippet payload))
         | 4 -> Logs.err (fun m -> m "suppressed background internal error: %s" (log_snippet payload))
         | _ -> Logs.debug (fun m -> m "suppressed background message kind=%d: %s" kind (log_snippet payload)));
        loop ()
      end
      else begin
        match kind with
        | 0 | 1 ->
          let buf = Buffer.create 256 in
          Buffer.add_string buf payload;
          drain buf
        | 5 ->
          let was_suppressing = t.suppressing in
          handle_done ();
          if was_suppressing then
            Logs.info (fun m -> m "background eval completed");
          if t.busy || was_suppressing then loop () else Done
        | _ ->
          (match kind with
           | 4 -> Logs.err (fun m -> m "received internal error: %s" (log_snippet payload))
           | 8 -> Logs.info (fun m -> m "entering passthrough mode")
           | 9 -> Logs.info (fun m -> m "leaving passthrough mode")
           | 11 -> Logs.info (fun m -> m "readline requested: %s" (log_snippet payload))
           | _ -> ());
          map_kind kind payload
      end
  in
  loop ()

let signal_passthrough () = Rffi.signal_passthrough ()
let submit_readline_input input = Rffi.submit_readline_input input
let cancel _t = ignore (Rffi.interrupt ())
let request_completions t input ~cursor_pos =
  if t.ready && not t.busy then
    Rffi.request_completions input cursor_pos

let request_columns t object_name =
  if t.ready && not t.busy then Rffi.request_columns object_name

let deinit _t = Rffi.shutdown ()
