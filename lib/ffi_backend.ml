type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown
  | Restarted of string
  | Passthrough
  | Passthrough_end
  | Completions of string list

type completion = string


type t = {
  mutable ready : bool;
  mutable busy : bool;
  mutable suppressing : bool;
  pending : [ `User of string | `Background of string ] Queue.t;
  mutable stashed : (int * int * string) option;
}

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
          "/usr/local/lib/R";
        ]
      in
      match List.find_opt has_libr candidates with
      | Some h -> h
      | None -> failwith "R_HOME not set and could not discover libR")

let start_eval t code =
  t.busy <- true;
  Rffi.submit code

let flush_pending t =
  match Queue.pop t.pending with
  | `User code ->
    t.suppressing <- false;
    start_eval t code
  | `Background code ->
    t.suppressing <- true;
    start_eval t code
  | exception Queue.Empty -> ()

let create ~sw () =
  let home = r_home () in
  let t = { ready = false; busy = false; suppressing = false;
            pending = Queue.create (); stashed = None } in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rc = Eio_unix.run_in_systhread (fun () -> Rffi.init home) in
    if rc <> 0 then failwith "Failed to initialize R runtime";
    t.ready <- true;
    flush_pending t;
    `Stop_daemon);
  t

let poll_ready t = t.ready

let submit t code =
  t.suppressing <- false;
  if t.ready && not t.busy then start_eval t code
  else Queue.push (`User code) t.pending

let background_submit t code =
  if t.ready && not t.busy then begin
    t.suppressing <- true;
    start_eval t code
  end else
    Queue.push (`Background code) t.pending

let map_kind kind payload =
  match kind with
  | 0 (* STDOUT *)         -> Stdout payload
  | 1 (* STDERR *)         -> Stdout payload
  | 2 (* RESULT *)         -> Result payload
  | 3 (* R_ERROR *)        -> R_error payload
  | 4 (* INTERNAL_ERROR *) -> Internal_error payload
  | 5 (* DONE *)           -> Done
  | 8 (* PASSTHROUGH *)    -> Passthrough
  | 9 (* PASSTHROUGH_END *)-> Passthrough_end
  | 10 (* COMPLETIONS *)   ->
    let items =
      if String.length payload = 0 then []
      else String.split_on_char '\n' payload
    in
    Completions items
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
    Eio.Fiber.yield ();
    match pop () with
    | None -> loop ()
    | Some (kind, _flags, payload) ->
      if t.suppressing && kind <> 5 && kind <> 8 && kind <> 9 && kind <> 10 then loop ()
      else begin
        match kind with
        | 0 | 1 ->
          let buf = Buffer.create 256 in
          Buffer.add_string buf payload;
          drain buf
        | 5 ->
          let was_suppressing = t.suppressing in
          handle_done ();
          if t.busy || was_suppressing then loop () else Done
        | _ -> map_kind kind payload
      end
  in
  loop ()

let signal_passthrough () = Rffi.signal_passthrough ()
let cancel _t = ignore (Rffi.interrupt ())
let request_completions t input ~cursor_pos =
  if t.ready && not t.busy then
    Rffi.request_completions input cursor_pos
let restart _t = ()
let deinit _t = Rffi.shutdown ()
