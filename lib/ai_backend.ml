(* Phase 1 AI backend.

   Spawns `claude -p` as a subprocess per query and streams its stdout
   (newline-delimited JSON events from --output-format stream-json) back
   through an Eio stream, so the main event loop can race [await_response] as a
   fourth fiber alongside the R backend.

   Phase 1 = talk to the AI, no tools: we disable Claude Code's built-in tools
   (--tools ""), ignore all globally-configured MCP servers (--strict-mcp-config
   with no --mcp-config), and replace the heavy default Claude Code system
   prompt with a focused one. The AI answers purely from the prompt text. *)

let system_prompt =
  "You are an assistant embedded in raoui, an interactive R REPL. The user \
   asks about their R session, output, and data analysis. Answer concisely and \
   practically. You have no tools available; reason only from what the user \
   tells you."

(* claude -p argv for a tool-less, MCP-less, streaming query. *)
let claude_args query =
  [
    "claude";
    "-p";
    query;
    "--output-format";
    "stream-json";
    "--verbose";
    "--tools";
    "";
    (* disable all built-in tools *)
    "--strict-mcp-config";
    (* ignore globally-configured MCP servers *)
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

let run_claude ~process_mgr ~chunks query =
  Eio.Switch.run @@ fun sw ->
  (* Merge the child's stdout and stderr into one pipe: stream-json lands on
     stdout, diagnostics on stderr, and non-JSON lines are simply ignored. *)
  let source, sink = Eio_unix.pipe sw in
  let _child =
    Eio.Process.spawn ~sw process_mgr ~stdout:sink ~stderr:sink
      (claude_args query)
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

let create ~sw ~process_mgr () =
  let chunks = Eio.Stream.create 1024 in
  let submit query =
    Eio.Fiber.fork ~sw (fun () ->
        (* Subprocess boundary: surface any failure as AI output rather than
           letting the fiber's exception tear down the switch (and the REPL). *)
        (try run_claude ~process_mgr ~chunks query
         with exn ->
           Eio.Stream.add chunks
             (Ffi_backend.Ai_output
                (Printf.sprintf "[ai error] %s\n" (Printexc.to_string exn))));
        Eio.Stream.add chunks Ffi_backend.Ai_done)
  in
  { chunks; submit }

let submit_query t query = t.submit query
let await_response t = Eio.Stream.take t.chunks
