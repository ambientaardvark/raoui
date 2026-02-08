type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown
  | Restarted of string

type completion = string


type t = {
  sw : Eio.Switch.t;
  mutable busy : bool;
  mutable suppressing : bool;
  mutable pending_bg : string list;
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

let create ~sw () =
  let home = r_home () in
  let rc = Rffi.init home in
  if rc <> 0 then failwith "Failed to initialize R runtime";
  { sw; busy = false; suppressing = false; pending_bg = []; stashed = None }

let poll_ready _t = true

let start_eval t code =
  t.busy <- true;
  Rffi.rb_reset ();
  Eio.Fiber.fork_daemon ~sw:t.sw (fun () ->
    Eio_unix.run_in_systhread (fun () -> ignore (Rffi.eval code));
    `Stop_daemon)

let submit t code =
  t.suppressing <- false;
  start_eval t code

let background_submit t code =
  if t.busy then t.pending_bg <- t.pending_bg @ [ code ]
  else begin
    t.suppressing <- true;
    start_eval t code
  end

let flush_pending t =
  match t.pending_bg with
  | [] -> ()
  | code :: rest ->
    t.pending_bg <- rest;
    t.suppressing <- true;
    start_eval t code

let map_kind kind payload =
  match kind with
  | 0 (* STDOUT *)         -> Stdout payload
  | 1 (* STDERR *)         -> Stdout payload
  | 2 (* RESULT *)         -> Result payload
  | 3 (* R_ERROR *)        -> R_error payload
  | 4 (* INTERNAL_ERROR *) -> Internal_error payload
  | 5 (* DONE *)           -> Done
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
      if t.suppressing && kind <> 5 then loop ()
      else begin
        match kind with
        | 0 | 1 ->
          let buf = Buffer.create 256 in
          Buffer.add_string buf payload;
          drain buf
        | 5 ->
          handle_done ();
          if t.busy then loop () else Done
        | _ -> map_kind kind payload
      end
  in
  loop ()

let cancel _t = ignore (Rffi.interrupt ())
let get_completions _t _input ~cursor_pos:_ = []
let restart _t = ()
let deinit _t = Rffi.shutdown ()
