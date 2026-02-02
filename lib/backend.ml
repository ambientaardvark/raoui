open Base

let log message =
  let oc =
    Stdlib.open_out_gen [ Open_append; Open_creat ] 0o666 "debug_log.txt"
  in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Stdlib.Printf.fprintf oc "%s\n" message)

type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown

type completion = string

type submission = User of string | Background of string

type t = {
  ctx : Zmq.Context.t;
  shell : [ `Dealer ] Zmq.Socket.t;
  iopub : [ `Sub ] Zmq.Socket.t;
  key : string;
  ark_pid : Core.Pid.t;
  session_id : string;
  mutable saw_busy : bool;
  mutable surpressing_responses : bool;
  mutable ready : bool;
  mutable pending_submit : submission option;
  connection_file : string;
}

let exe_dir () =
  Stdlib.Filename.dirname (Caml_unix.realpath Stdlib.Sys.executable_name)

let kernel_path () =
  (* Check for ark next to executable first (bundle case) *)
  let bundled_ark = Stdlib.Filename.concat (exe_dir ()) "ark" in
  if Stdlib.Sys.file_exists bundled_ark then bundled_ark
  else
    (* Fall back to vendor paths relative to cwd (dev case) *)
    let candidates =
      [
        "vendor/ark-0.1.227-linux-x64/ark";
        "vendor/ark-0.1.223-darwin-universal/ark";
      ]
    in
    match List.find candidates ~f:Stdlib.Sys.file_exists with
    | Some path -> path
    | None ->
        failwith
          "No ark binary found. Expected either next to executable or in \
           vendor/"

let make_connection_file key = Printf.sprintf "/tmp/kernel%s.json" key

let startup_file () =
  let bundled = Stdlib.Filename.concat (exe_dir ()) "startup.R" in
  if Stdlib.Sys.file_exists bundled then bundled else "r_scripts/startup.R"

let random_hex_token len =
  let hex_chars = "0123456789abcdef" in
  String.init (len * 2) ~f:(fun _ -> hex_chars.[Random.int 16])

let get_available_port () =
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.bind sock (ADDR_INET (Unix.inet_addr_any, 0));
  let port =
    match Unix.getsockname sock with
    | ADDR_INET (_, port) -> port
    | _ -> failwith "Could not get socket port"
  in
  Unix.close sock;
  port

let conn_info () =
  let token_hex = random_hex_token 16 in
  `Assoc
    [
      ("ip", `String "127.0.0.1");
      ("transport", `String "tcp");
      ("shell_port", `Int (get_available_port ()));
      ("iopub_port", `Int (get_available_port ()));
      ("stdin_port", `Int (get_available_port ()));
      ("control_port", `Int (get_available_port ()));
      ("hb_port", `Int (get_available_port ()));
      ("key", `String token_hex);
      ("signature_scheme", `String "hmac-sha256");
      ("kernel_name", `String "ark");
    ]

let start_kernel connection_file =
  let preexec_fn () =
    let dev_null = Caml_unix.openfile "/dev/null" [ O_WRONLY ] 0 in
    Caml_unix.dup2 dev_null Caml_unix.stdout;
    Caml_unix.dup2 dev_null Caml_unix.stderr;
    Caml_unix.close dev_null
  in
  let kernel_path = kernel_path () in
  Core_unix.fork_exec ~prog:kernel_path ~preexec_fn
    ~argv:
      [
        kernel_path;
        "--connection_file";
        connection_file;
        "--session-mode";
        "console";
        "--startup-file";
        startup_file ();
      ]
    ()

let sign parts key =
  let h = Cryptokit.MAC.hmac_sha256 key in
  List.iter parts ~f:(fun x -> h#add_string x);
  Cryptokit.transform_string (Cryptokit.Hexa.encode ()) h#result

let make_header msg_type t =
  let uuid = random_hex_token 16 in
  let date =
    Core_unix.time () |> Ptime.of_float_s
    |> Option.value_exn ~message:"invalid date"
    |> Ptime.to_rfc3339
  in
  `Assoc
    [
      ("msg_id", `String uuid);
      ("msg_type", `String msg_type);
      ("username", `String "user");
      ("session", `String t.session_id);
      ("version", `String "5.4");
      ("date", `String date);
    ]

let send_message t socket msg_type content =
  let header = make_header msg_type t |> Yojson.Safe.to_string in
  let content = Yojson.Safe.to_string content in
  log (Printf.sprintf "sending header:\n %s\n\n content:\n%s" header content);
  let parent_header = "{}" in
  let metadata = "{}" in
  let signature = sign [ header; parent_header; metadata; content ] t.key in
  Zmq.Socket.send_all socket
    [ ""; "<IDS|MSG>"; signature; header; parent_header; metadata; content ]

let create () =
  let conn_info = conn_info () in
  let key = Yojson.Basic.Util.(conn_info |> member "key" |> to_string) in
  let connection_file = make_connection_file key in
  Stdio.Out_channel.write_all connection_file
    ~data:(Yojson.Safe.to_string conn_info);

  let ark_pid =
    Eio_unix.run_in_systhread (fun () -> start_kernel connection_file)
  in

  (* Connect to ark kernel *)
  let ctx = Zmq.Context.create () in
  let shell = Zmq.Socket.create ctx Zmq.Socket.dealer in
  let iopub = Zmq.Socket.create ctx Zmq.Socket.sub in

  Zmq.Socket.connect shell
    (Printf.sprintf "tcp://127.0.0.1:%d"
       Yojson.Basic.Util.(conn_info |> member "shell_port" |> to_int));
  Zmq.Socket.connect iopub
    (Printf.sprintf "tcp://127.0.0.1:%d"
       Yojson.Basic.Util.(conn_info |> member "iopub_port" |> to_int));
  Zmq.Socket.subscribe iopub "";
  let session_id = random_hex_token 16 in
  {
    ctx;
    shell;
    iopub;
    ark_pid;
    key;
    session_id;
    saw_busy = false;
    surpressing_responses = false;
    ready = false;
    pending_submit = None;
    connection_file;
  }

let recv_message socket =
  let message = Zmq.Socket.recv_all socket ~block:false in
  let rec read_message parts =
    match parts with
    | delim :: _signature :: header :: _parent_h :: _metadata :: content
      :: _buffers
      when String.equal delim "<IDS|MSG>" ->
        (header, content)
    | _hd :: tl -> read_message tl
    | [] -> failwith "malformed response"
  in
  let header, content = read_message message in
  log content;
  Yojson.Basic.(from_string header, from_string content)

let send_execute_request t input =
  let code = match input with
  | User c -> c
  | Background c -> t.surpressing_responses <- true; c
  in
  let content =
    `Assoc
      [
        ("code", `String code);
        ("silent", `Bool false);
        ("store_history", `Bool true);
        ("user_expressions", `Assoc []);
        ("allow_stdin", `Bool false);
        ("stop_on_error", `Bool true);
      ]
  in
  send_message t t.shell "execute_request" content

let flush_pending t =
  match t.pending_submit with
  | Some input ->
      t.pending_submit <- None;
      send_execute_request t input
  | None -> ()

let poll_ready t =
  if t.ready then true
  else
    let open Yojson.Basic.Util in
    let rec drain () =
      match Zmq.Socket.recv_all ~block:false t.iopub with
      | message -> (
          let rec read_message parts =
            match parts with
            | delim :: _signature :: header :: _parent_h :: _metadata :: _content
              :: _buffers
              when String.equal delim "<IDS|MSG>" ->
                header
            | _ :: tl -> read_message tl
            | [] -> failwith "malformed response"
          in
          let header = read_message message in
          let header = Yojson.Basic.from_string header in
          let msg_type = header |> member "msg_type" |> to_string in
          match msg_type with
          | "iopub_welcome" ->
              t.ready <- true;
              flush_pending t
          | _ -> drain ())
      | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
    in
    drain ();
    t.ready

let submit t input =
  if t.ready then send_execute_request t (User input)
  else t.pending_submit <- Some (User input)

let background_submit t input =
  if t.ready then send_execute_request t (Background input)
  else t.pending_submit <- Some (Background input)


let parse_response response_field =
  match Yojson.Basic.Util.member "text/plain" response_field with
  | `Null ->
      failwith
        (Printf.sprintf
           "Error parsing backend response: need to add more logic to handle %s"
           (Yojson.Basic.Util.to_string response_field))
  | output -> Yojson.Basic.Util.to_string output

let pretty_print_error error_data =
  Yojson.Basic.Util.(error_data |> member "evalue" |> to_string)

let await_response t =
  let open Yojson.Basic.Util in
  let rec loop () =
    Eio.Fiber.yield ();
    match recv_message t.iopub with
    | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> raise Stdlib.Exit
    | (header, content) -> (
        let message_type = header |> member "msg_type" |> to_string in
        log (Printf.sprintf "recv: %s (saw_busy=%b)" message_type t.saw_busy);
        match message_type with
        | "status" -> (
            let new_status = content |> member "execution_state" |> to_string in
            log (Printf.sprintf "  status: %s" new_status);
            match new_status with
            | "starting" -> loop ()
            | "busy" ->
                t.saw_busy <- true;
                loop ()
            | "idle" ->
                t.surpressing_responses <- false;
                if t.saw_busy then (t.saw_busy <- false; Done) else loop ()
            | s ->
                Internal_error
                  (Printf.sprintf "Unexpected status from backend: %s" s))
        | "stream" -> (
            if t.surpressing_responses then loop ()
            else
              let to_print = content |> member "text" |> to_string in
              match content |> member "name" |> to_string with
              | "stdout" -> Stdout to_print
              | "stderr" -> loop () (* skip ark's internal logging *)
              | _ -> Internal_error "invalid response channel")
        | "execute_result" ->
            if t.surpressing_responses then loop ()
            else Result (content |> member "data" |> parse_response)
        | "error" ->
            if t.surpressing_responses then loop ()
            else R_error (pretty_print_error content)
        | "iopub_welcome" ->
            t.ready <- true;
            flush_pending t;
            loop ()
        | _ -> loop ())
  in
  try loop () with
  | Stdlib.Exit -> Shutdown
  | e ->
      Internal_error (Printf.sprintf "Backend exception: %s" (Exn.to_string e))

let cancel t = Caml_unix.kill (Core.Pid.to_int t.ark_pid) Stdlib.Sys.sigint
let get_completions _t _input ~cursor_pos:_ = []

let deinit t =
  (* NOTE: sigterm won't work on windows. need to make this more robust *)
  Caml_unix.kill (Core.Pid.to_int t.ark_pid) Stdlib.Sys.sigterm;
  Zmq.Socket.close t.shell;
  Zmq.Socket.close t.iopub;
  Zmq.Context.terminate t.ctx;
  Unix.unlink t.connection_file
