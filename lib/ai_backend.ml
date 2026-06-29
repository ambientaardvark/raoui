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
   output when that context would help, or the search_history tool to find past \
   commands and output matching a keyword. Use the run_r tool to evaluate R \
   against the user's live session in a read-only sandbox (changes do not \
   persist) to inspect data or compute. For code meant to change the session \
   (assignments to keep, writing files, plotting), do not run it — call \
   suggest_code to place it in the user's prompt for them to run themselves."

(* Inline --mcp-config JSON pointing claude at our loopback MCP server. *)
let mcp_config mcp_port =
  Printf.sprintf
    {|{"mcpServers":{"raoui":{"type":"http","url":"http://127.0.0.1:%d/mcp"}}}|}
    mcp_port

(* Random v4 UUID for the claude session id. *)
let random_uuid () =
  let b = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set_uint8 b i (Random.int 256)
  done;
  Bytes.set_uint8 b 6 ((Bytes.get_uint8 b 6 land 0x0f) lor 0x40);
  Bytes.set_uint8 b 8 ((Bytes.get_uint8 b 8 land 0x3f) lor 0x80);
  let h i = Printf.sprintf "%02x" (Bytes.get_uint8 b i) in
  Printf.sprintf "%s%s%s%s-%s%s-%s%s-%s%s-%s%s%s%s%s%s" (h 0) (h 1) (h 2) (h 3)
    (h 4) (h 5) (h 6) (h 7) (h 8) (h 9) (h 10) (h 11) (h 12) (h 13) (h 14) (h 15)

(* claude -p argv: built-ins off, only raoui's MCP tools, streaming. [session]
   carries the session flags (--session-id on the first query, --resume after),
   so the conversation persists across queries within this raoui run. *)
let claude_args ~mcp_port ~session query =
  [ "claude"; "-p"; query; "--output-format"; "stream-json"; "--verbose" ]
  @ session
  @ [
      "--tools";
      "";
      (* disable all built-in tools (Bash/Read/Write/...) *)
      "--mcp-config";
      mcp_config mcp_port;
      "--strict-mcp-config";
      (* ignore globally-configured MCP servers; use only ours *)
      "--allowedTools";
      "mcp__raoui__get_history";
      "mcp__raoui__search_history";
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
  (* Current conversation's claude session id. *)
  session_id : string ref;
  (* Whether the session has been created yet (first query) — picks
     --session-id vs --resume. *)
  started : bool ref;
}

(* Look up [key] in a JSON object; None for non-objects or missing keys. *)
let field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let block_type b =
  match field "type" b with Some (`String s) -> Some s | _ -> None

(* mcp__raoui__run_r -> run_r; leave anything unexpected as-is. *)
let short_tool_name name =
  let prefix = "mcp__raoui__" in
  let pl = String.length prefix in
  if String.length name >= pl && String.sub name 0 pl = prefix then
    String.sub name pl (String.length name - pl)
  else name

(* Split one assistant event into the chunks it should produce. A message that
   invokes tools is a "tool turn": we surface a compact Ai_tool_call per tool
   and SUPPRESS its accompanying narration text (the "I'll now check..."
   preamble). A text-only message is the actual answer and renders as prose.
   thinking blocks are dropped. *)
let assistant_chunks json =
  match field "type" json with
  | Some (`String "assistant") -> (
      match field "message" json with
      | Some msg -> (
          match field "content" msg with
          | Some (`List blocks) ->
              let tool_calls =
                blocks
                |> List.filter_map (fun b ->
                       if block_type b = Some "tool_use" then
                         match field "name" b with
                         | Some (`String n) ->
                             Some (Ffi_backend.Ai_tool_call (short_tool_name n))
                         | _ -> None
                       else None)
              in
              if tool_calls <> [] then tool_calls
              else
                let text =
                  blocks
                  |> List.filter_map (fun b ->
                         match (block_type b, field "text" b) with
                         | Some "text", Some (`String t) -> Some t
                         | _ -> None)
                  |> String.concat ""
                in
                if String.trim text = "" then []
                else
                  let text =
                    if text.[String.length text - 1] = '\n' then text
                    else text ^ "\n"
                  in
                  [ Ffi_backend.Ai_output text ]
          | _ -> [])
      | None -> [])
  | _ -> []

(* Parse one output line and push its chunks. Non-JSON lines (e.g. merged
   stderr) and non-assistant events yield nothing. *)
let handle_line chunks line =
  match Yojson.Safe.from_string line with
  | exception _ -> ()
  | json -> List.iter (Eio.Stream.add chunks) (assistant_chunks json)

let run_claude ~process_mgr ~chunks ~mcp_port ~session query =
  Eio.Switch.run @@ fun sw ->
  (* Merge the child's stdout and stderr into one pipe: stream-json lands on
     stdout, diagnostics on stderr, and non-JSON lines are simply ignored. *)
  let source, sink = Eio_unix.pipe sw in
  let _child =
    Eio.Process.spawn ~sw process_mgr ~stdout:sink ~stderr:sink
      (claude_args ~mcp_port ~session query)
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
  Random.self_init ();
  let session_id = ref (random_uuid ()) in
  let started = ref false in
  let submit query =
    (* First query creates the session; later ones resume it for continuity. *)
    let session =
      if !started then [ "--resume"; !session_id ]
      else [ "--session-id"; !session_id ]
    in
    started := true;
    Eio.Fiber.fork ~sw (fun () ->
        (* Subprocess boundary: surface any failure as AI output rather than
           letting the fiber's exception tear down the switch (and the REPL). *)
        (try run_claude ~process_mgr ~chunks ~mcp_port ~session query
         with exn ->
           Eio.Stream.add chunks
             (Ffi_backend.Ai_output
                (Printf.sprintf "[ai error] %s\n" (Printexc.to_string exn))));
        Eio.Stream.add chunks Ffi_backend.Ai_done)
  in
  { chunks; submit; session_id; started }

let submit_query t query = t.submit query

(* Start a fresh conversation: new session id, and a notice the loop renders. *)
let reset t =
  t.session_id := random_uuid ();
  t.started := false;
  Eio.Stream.add t.chunks
    (Ffi_backend.Ai_output "[started a new AI conversation]\n");
  Eio.Stream.add t.chunks Ffi_backend.Ai_done

let await_response t = Eio.Stream.take t.chunks
