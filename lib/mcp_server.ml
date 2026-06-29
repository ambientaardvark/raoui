(* In-process MCP server exposing raoui's session to the `claude -p` subprocess
   over loopback HTTP (MCP streamable-HTTP transport).

   Phase-2 read-only start: a single [get_history] tool backed by
   [History.recent_interactions]. The server lives as a fiber on the caller's
   switch for the whole session and serves every `claude -p` invocation; the
   chosen port is handed to [Ai_backend] so it can point claude at us.

   The transport is deliberately minimal: each JSON-RPC request is answered with
   a single application/json body (no server->client SSE stream); notifications
   are acked with 202; a client GET probe for an SSE stream is refused with 405.
   This is enough for the CLI's MCP client (verified against claude 2.1.195). *)

let protocol_version = "2025-06-18"
let session_id = "raoui-session"
let default_limit = 20

(* JSON object field lookup; None for non-objects / missing keys. *)
let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let ok_result id result =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

let error_result id code msg =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", id);
      ("error", `Assoc [ ("code", `Int code); ("message", `String msg) ]);
    ]

(* Schema advertised by tools/list for the one read-only tool. *)
let get_history_schema =
  `Assoc
    [
      ("name", `String "get_history");
      ( "description",
        `String
          "Return the user's most recent raoui REPL interactions: each R (or \
           shell) command they submitted and its output. Use this to ground \
           answers about what the user has been doing in their session." );
      ( "inputSchema",
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ( "limit",
                    `Assoc
                      [
                        ("type", `String "integer");
                        ( "description",
                          `String
                            "Maximum number of recent interactions to return \
                             (default 20)." );
                      ] );
                ] );
          ] );
    ]

(* Schema for the sandboxed R evaluator. *)
let run_r_schema =
  `Assoc
    [
      ("name", `String "run_r");
      ( "description",
        `String
          "Evaluate R code in a sandboxed fork of the user's live session and \
           return its printed output. The fork sees the user's current \
           variables, but it is read-only: file writes, network, and system() \
           are blocked, and any changes do NOT persist to the real session. \
           Use this to inspect data and compute. For code meant to CHANGE the \
           session (assignments to keep, writing files, plots), do not run it \
           — propose it for the user to run instead." );
      ( "inputSchema",
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ( "code",
                    `Assoc
                      [
                        ("type", `String "string");
                        ("description", `String "R code to evaluate.");
                      ] );
                ] );
            ("required", `List [ `String "code" ]);
          ] );
    ]

(* Render one output chunk for the model; plots collapse to a placeholder. *)
let format_output (o : History.output) =
  match o.text with
  | Some text -> text
  | None -> ( match o.image_path with Some _ -> "[plot]" | None -> "")

(* Render one interaction as its mode-prefixed input plus its text outputs. *)
let format_interaction (it : History.interaction) =
  let body =
    it.outputs
    |> List.filter_map (fun o ->
           match format_output o with "" -> None | s -> Some s)
    |> String.concat "\n"
  in
  Printf.sprintf "[%s] %s%s" it.mode it.input
    (if body = "" then "" else "\n" ^ body)

(* The get_history tool body: newest-first from the DB, reversed to read
   chronologically (oldest first, most recent last). *)
let get_history history ~limit =
  match History.recent_interactions history ~limit |> List.rev with
  | [] -> "(no history yet)"
  | interactions ->
      interactions
      |> List.mapi (fun i it ->
             Printf.sprintf "%d. %s" (i + 1) (format_interaction it))
      |> String.concat "\n\n"

(* Pull the integer [limit] argument out of a tools/call request. *)
let call_limit json =
  match field "params" json with
  | Some params -> (
      match field "arguments" params with
      | Some args -> (
          match field "limit" args with
          | Some (`Int n) -> n
          | Some (`Intlit s) -> int_of_string s
          | _ -> default_limit)
      | None -> default_limit)
  | None -> default_limit

(* Pull a required string argument [key] out of a tools/call request. *)
let call_string_arg key json =
  match field "params" json with
  | Some params -> (
      match field "arguments" params with
      | Some args -> (
          match field key args with Some (`String s) -> Some s | _ -> None)
      | None -> None)
  | None -> None

(* Evaluate R code in the sandboxed fork. Run on a systhread so the blocking
   wait for the worker's fork/eval/reap doesn't freeze the Eio event loop. *)
let run_r code =
  let output = Eio_unix.run_in_systhread (fun () -> Rffi.run_r_sandboxed code) in
  if output = "" then "(no output)" else output

(* Wrap a tool's text output as an MCP tools/call result. *)
let tool_text_result text =
  `Assoc
    [
      ( "content",
        `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ] );
      ("isError", `Bool false);
    ]

(* Build a JSON-RPC response for one request object. None => notification
   (no reply body, HTTP 202). *)
let handle_rpc history json =
  let id = Option.value ~default:`Null (field "id" json) in
  match field "method" json with
  | Some (`String "initialize") ->
      Some
        (ok_result id
           (`Assoc
              [
                ("protocolVersion", `String protocol_version);
                ("capabilities", `Assoc [ ("tools", `Assoc []) ]);
                ( "serverInfo",
                  `Assoc
                    [ ("name", `String "raoui"); ("version", `String "0.1") ] );
              ]))
  | Some (`String "notifications/initialized") -> None
  | Some (`String "tools/list") ->
      Some
        (ok_result id
           (`Assoc [ ("tools", `List [ get_history_schema; run_r_schema ]) ]))
  | Some (`String "tools/call") -> (
      match field "params" json with
      | Some params -> (
          match field "name" params with
          | Some (`String "get_history") ->
              let limit = call_limit json in
              Some
                (ok_result id
                   (tool_text_result (get_history history ~limit)))
          | Some (`String "run_r") -> (
              match call_string_arg "code" json with
              | Some code -> Some (ok_result id (tool_text_result (run_r code)))
              | None ->
                  Some
                    (error_result id (-32602)
                       "run_r requires a 'code' string argument"))
          | Some (`String other) ->
              Some (error_result id (-32602) ("unknown tool: " ^ other))
          | _ -> Some (error_result id (-32602) "missing tool name"))
      | None -> Some (error_result id (-32602) "missing params"))
  | Some (`String other) ->
      Some (error_result id (-32601) ("method not found: " ^ other))
  | _ -> Some (error_result id (-32600) "invalid request")

let json_headers =
  Http.Header.of_list
    [ ("content-type", "application/json"); ("mcp-session-id", session_id) ]

let handler history _socket request body =
  match Http.Request.meth request with
  | `POST -> (
      let raw = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int in
      let json = Yojson.Safe.from_string raw in
      match handle_rpc history json with
      | Some response ->
          Cohttp_eio.Server.respond_string ~headers:json_headers ~status:`OK
            ~body:(Yojson.Safe.to_string response) ()
      | None -> Cohttp_eio.Server.respond_string ~status:`Accepted ~body:"" ())
  | _ ->
      (* No server->client SSE stream; refuse GET and everything else. *)
      Cohttp_eio.Server.respond_string ~status:`Method_not_allowed ~body:"" ()

(* Bind a loopback socket on an OS-assigned port, fork the server fiber on [sw],
   and return the chosen port. *)
let start ~sw ~net ~history =
  let socket =
    Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let server = Cohttp_eio.Server.make ~callback:(handler history) () in
  (* Daemon, not a plain fiber: the accept loop never returns, so it must be
     cancelled when the session switch closes or shutdown would hang. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun exn ->
          Logs.warn (fun m ->
              m "mcp server connection error: %s" (Printexc.to_string exn))));
  Logs.info (fun m -> m "mcp server listening on 127.0.0.1:%d" port);
  port
