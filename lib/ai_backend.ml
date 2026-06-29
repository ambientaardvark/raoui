(* AI backend.

   Spawns `claude -p` as a subprocess per query and streams its stdout
   (newline-delimited JSON events from --output-format stream-json) back
   through an Eio stream, so the main event loop can race [await_response] as a
   fourth fiber alongside the R backend.

   Tools come solely from raoui's in-process MCP server (see [Mcp_server]),
   reached over loopback at [mcp_port]: we disable Claude Code's built-in tools
   (--tools ""), ignore all globally-configured MCP servers (--strict-mcp-config),
   point claude at our server (--mcp-config), pre-approve its read-only tools
   (--allowedTools) so the non-interactive `-p` run never blocks on a prompt,
   and replace the heavy default Claude Code system prompt with a focused one. *)

let system_prompt =
  "You are an assistant embedded in raoui, an interactive R REPL. The user \
   asks about their R session, output, and data analysis. Answer concisely and \
   practically. Use the get_history tool to see the user's recent commands and \
   output when that context would help. Use the run_r tool to evaluate R \
   against the user's live session in a read-only sandbox (changes do not \
   persist) to inspect data or compute. For code meant to change the session \
   (assignments to keep, writing files, plotting), do not run it — call \
   suggest_code to place it in the user's prompt for them to run themselves."

(* Inline --mcp-config JSON pointing claude at our loopback MCP server. *)
let mcp_config mcp_port =
  Printf.sprintf
    {|{"mcpServers":{"raoui":{"type":"http","url":"http://127.0.0.1:%d/mcp"}}}|}
    mcp_port

(* claude -p argv: built-ins off, only raoui's MCP tools, streaming. *)
let claude_args ~mcp_port query =
  [
    "claude";
    "-p";
    query;
    "--output-format";
    "stream-json";
    "--verbose";
    "--tools";
    "";
    (* disable all built-in tools (Bash/Read/Write/...) *)
    "--mcp-config";
    mcp_config mcp_port;
    "--strict-mcp-config";
    (* ignore globally-configured MCP servers; use only ours *)
    "--allowedTools";
    "mcp__raoui__get_history";
    "mcp__raoui__run_r";
    "mcp__raoui__suggest_code";
    (* pre-approve our tools so the non-interactive -p run never blocks *)
    "--system-prompt";
    system_prompt;
  ]

type t = {
  (* Pending AI output chunks awaiting render by the event loop. *)
  chunks : Ffi_backend.response_chunk Eio.Stream.t;
  (* Spawn `claude -p` for a query and stream its output into [chunks]. *)
  submit : string -> unit;
}

(* Look up [key] in a JSON object; None for non-objects or missing keys. *)
let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

(* Concatenated assistant text from one stream-json event, or None when the
   line is not an assistant message. *)
let assistant_text json =
  match field "type" json with
  | Some (`String "assistant") -> (
      match field "message" json with
      | Some msg -> (
          match field "content" msg with
          | Some (`List blocks) ->
              blocks
              |> List.filter_map (fun b ->
                     match (field "type" b, field "text" b) with
                     | Some (`String "text"), Some (`String t) -> Some t
                     | _ -> None)
              |> String.concat "" |> Option.some
          | _ -> None)
      | None -> None)
  | _ -> None

(* Parse one output line and push any assistant text as a chunk. Non-JSON lines
   (e.g. merged stderr) and non-assistant events are ignored. *)
let handle_line chunks line =
  match Yojson.Safe.from_string line with
  | exception _ -> ()
  | json -> (
      match assistant_text json with
      | Some text when text <> "" ->
          let text =
            if text.[String.length text - 1] = '\n' then text else text ^ "\n"
          in
          Eio.Stream.add chunks (Ffi_backend.Ai_output text)
      | _ -> ())

let run_claude ~process_mgr ~chunks ~mcp_port query =
  Eio.Switch.run @@ fun sw ->
  (* Merge the child's stdout and stderr into one pipe: stream-json lands on
     stdout, diagnostics on stderr, and non-JSON lines are simply ignored. *)
  let source, sink = Eio_unix.pipe sw in
  let _child =
    Eio.Process.spawn ~sw process_mgr ~stdout:sink ~stderr:sink
      (claude_args ~mcp_port query)
  in
  (* Close our copy of the write end so [source] sees EOF when the child exits. *)
  Eio.Flow.close sink;
  let buf = Eio.Buf_read.of_flow source ~max_size:(64 * 1024 * 1024) in
  let rec read_lines () =
    match Eio.Buf_read.line buf with
    | line ->
        handle_line chunks line;
        read_lines ()
    | exception End_of_file -> ()
  in
  read_lines ()

let create ~sw ~process_mgr ~mcp_port ~chunks () =
  let submit query =
    Eio.Fiber.fork ~sw (fun () ->
        (* Subprocess boundary: surface any failure as AI output rather than
           letting the fiber's exception tear down the switch (and the REPL). *)
        (try run_claude ~process_mgr ~chunks ~mcp_port query
         with exn ->
           Eio.Stream.add chunks
             (Ffi_backend.Ai_output
                (Printf.sprintf "[ai error] %s\n" (Printexc.to_string exn))));
        Eio.Stream.add chunks Ffi_backend.Ai_done)
  in
  { chunks; submit }

let submit_query t query = t.submit query
let await_response t = Eio.Stream.take t.chunks
