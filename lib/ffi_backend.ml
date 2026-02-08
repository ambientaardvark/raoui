type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown
  | Restarted of string

type completion = string

let log message =
  let oc =
    Stdlib.open_out_gen [ Open_append; Open_creat ] 0o666 "/Users/alanlee/Documents/Programs/raoui/debug_log.txt"
  in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.Printf.fprintf oc "%s\n" message)

type t = {
  sw : Eio.Switch.t;
  mutable busy : bool;
  mutable suppressing : bool;
  mutable pending_bg : string list;
}

let r_home () =
  match Stdlib.Sys.getenv_opt "R_HOME" with
  | Some h -> h
  | None ->
    let default = "/Library/Frameworks/R.framework/Resources" in
    if Stdlib.Sys.file_exists default then default
    else failwith "R_HOME not set and no R installation found"

let create ~sw () =
  let home = r_home () in
  let rc = Rffi.init home in
  if rc <> 0 then failwith "Failed to initialize R runtime";
  { sw; busy = false; suppressing = false; pending_bg = [] }

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
  let rec loop () =
    Eio.Fiber.yield ();
    match Rffi.rb_pop () with
    | None -> loop ()
    | Some (kind, _flags, payload) ->
      log (Printf.sprintf "rb_pop: kind=%d len=%d payload=%S" kind (String.length payload) payload);
      if t.suppressing && kind <> 5 then loop ()
      else begin
        let chunk = map_kind kind payload in
        match chunk with
        | Done ->
          t.busy <- false;
          t.suppressing <- false;
          flush_pending t;
          if t.busy then loop () else Done
        | other -> other
      end
  in
  loop ()

let cancel _t = () (* TODO: interrupt R eval *)
let get_completions _t _input ~cursor_pos:_ = []
let restart _t = ()
let deinit _t = Rffi.shutdown ()
