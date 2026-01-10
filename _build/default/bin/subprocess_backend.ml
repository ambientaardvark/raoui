(** Subprocess-based R backend.

    Communicates with R via stdin/stdout of a child process.
    This is simpler than FFI and works well for most use cases. *)

open Lwt.Infix

type r_value = unit  (* placeholder for future FFI *)

(* Sentinel strings for detecting end of output *)
let output_sentinel = "##ROCAML_OUTPUT_END##"
let error_sentinel = "##ROCAML_ERROR_END##"

type t = {
  process : Lwt_process.process;
  mutable buffer : string;
}

let r_init_commands = {|options(prompt = "")
options(continue = "")
options(warn = 1)
options(crayon.enabled = FALSE)
options(cli.num_colors = 1)
.rocaml_output_end <- function() cat("##ROCAML_OUTPUT_END##\n")
.rocaml_error_end <- function() cat("##ROCAML_ERROR_END##\n", file = stderr())
|}

let start () =
  let cmd = ("/usr/local/bin/R", [| "/usr/local/bin/R"; "--vanilla"; "--interactive" |]) in
  let process = Lwt_process.open_process cmd in
  let t = { process; buffer = "" } in
  (* Send initialization commands *)
  Lwt_io.write process#stdin r_init_commands >>= fun () ->
  Lwt_io.flush process#stdin >>= fun () ->
  (* Drain any startup output *)
  let rec drain_output () =
    Lwt_io.read_line_opt process#stdout >>= function
    | Some _ -> drain_output ()
    | None -> Lwt.return ()
  in
  Lwt.pick [
    drain_output ();
    Lwt_unix.sleep 0.5
  ] >>= fun () ->
  Lwt.return t

let send_command t cmd =
  let wrapped = Printf.sprintf
    "tryCatch({ .result <- (%s); print(.result); .rocaml_output_end() }, error = function(e) { message(e$message); .rocaml_error_end() })\n"
    cmd
  in
  Lwt_io.write t.process#stdin wrapped >>= fun () ->
  Lwt_io.flush t.process#stdin

let read_until_sentinel t sentinel =
  let rec loop acc =
    Printf.eprintf "DEBUG: Waiting to read line...\n%!";
    Lwt_io.read_line_opt t.process#stdout >>= function
    | None -> 
      Printf.eprintf "DEBUG: EOF reached\n%!";
      Lwt.return (String.concat "\n" (List.rev acc))
    | Some line ->
      Printf.eprintf "DEBUG: Read line: %S\n%!" line;
      (* Strip ANSI codes and carriage returns *)
      let clean_line = 
        line 
        |> Str.global_replace (Str.regexp "\027\\[[0-9;]*m") ""
        |> Str.global_replace (Str.regexp "\r") ""
        |> String.trim
      in
      Printf.eprintf "DEBUG: Cleaned line: %S\n%!" clean_line;
      if clean_line = sentinel then begin
        Printf.eprintf "DEBUG: Found sentinel!\n%!";
        Lwt.return (String.concat "\n" (List.rev acc))
      end else if String.length clean_line > 0 && clean_line.[0] = '>' then begin
        (* Skip echo lines that start with > *)
        Printf.eprintf "DEBUG: Skipping echo line\n%!";
        loop acc
      end else
        loop (clean_line :: acc)
  in
  loop []

let eval t expr =
  Printf.eprintf "DEBUG: Sending command: %s\n%!" expr;
  send_command t expr >>= fun () ->
  Printf.eprintf "DEBUG: Command sent, waiting for output...\n%!";
  read_until_sentinel t output_sentinel >>= fun output ->
  Printf.eprintf "DEBUG: Got output: %s\n%!" output;
  let trimmed = String.trim output in
  if trimmed = "" then
    Lwt.return (R_backend.Output "")
  else
    Lwt.return (R_backend.Output trimmed)

let complete t ~prefix =
  (* Use R's built-in completion *)
  let cmd = Printf.sprintf
    {|{ utils:::.assignLinebuffer("%s")
utils:::.assignEnd(%d)
utils:::.completeToken()
utils:::.retrieveCompletions() }|}
    (String.escaped prefix)
    (String.length prefix)
  in
  eval t cmd >>= function
  | R_backend.Output s ->
    (* Parse the R character vector output *)
    let completions =
      s
      |> String.split_on_char '\n'
      |> List.filter_map (fun line ->
          let line = String.trim line in
          if String.length line > 4 && line.[0] = '[' then
            (* Skip the [1] prefix *)
            try
              let start = String.index line '"' + 1 in
              let stop = String.rindex line '"' in
              Some (String.sub line start (stop - start))
            with Not_found -> None
          else None)
    in
    Lwt.return completions
  | R_backend.Error _ | R_backend.Incomplete -> Lwt.return []

let is_complete t expr =
  (* Use R's parser to check if expression is complete *)
  let cmd = Printf.sprintf
    {|tryCatch(parse(text = "%s"), error = function(e) if (grepl("unexpected end", e$message)) "incomplete" else "error")|}
    (String.escaped expr)
  in
  eval t cmd >>= function
  | R_backend.Output s -> Lwt.return (not (String.equal (String.trim s) {|[1] "incomplete"|}))
  | _ -> Lwt.return true

let shutdown t =
  Lwt_io.write t.process#stdin "q('no')\n" >>= fun () ->
  Lwt_io.flush t.process#stdin >>= fun () ->
  t.process#terminate;
  Lwt.return ()

let get_value _t _name =
  (* Not implemented for subprocess backend *)
  Lwt.return None

module Backend : R_backend.S with type t = t = struct
  type nonrec t = t
  let start = start
  let eval = eval
  let complete = complete
  let is_complete = is_complete
  let shutdown = shutdown
  let get_value = get_value
end
