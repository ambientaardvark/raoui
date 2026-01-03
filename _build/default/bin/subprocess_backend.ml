(** Subprocess-based R backend.

    Communicates with R via stdin/stdout of a child process.
    This is simpler than FFI and works well for most use cases. *)

open Lwt.Infix

type eval_result =
  | Output of string
  | Error of string
  | Incomplete

type r_value = unit  (* placeholder for future FFI *)

(* Sentinel strings for detecting end of output *)
let output_sentinel = "##ROCAML_OUTPUT_END##"
let error_sentinel = "##ROCAML_ERROR_END##"

type t = {
  process : Lwt_process.process;
  mutable buffer : string;
}

let r_init_commands = {|
  options(prompt = "")
  options(continue = "")
  options(warn = 1)
  .rocaml_output_end <- function() cat("\n##ROCAML_OUTPUT_END##\n")
  .rocaml_error_end <- function() cat("\n##ROCAML_ERROR_END##\n", file = stderr())
|}

let start () =
  let cmd = ("R", [| "R"; "--interactive"; "--no-save"; "--no-restore"; "-q" |]) in
  let process = Lwt_process.open_process cmd in
  let t = { process; buffer = "" } in
  (* Send initialization commands *)
  Lwt_io.write process#stdin r_init_commands >>= fun () ->
  Lwt_io.flush process#stdin >>= fun () ->
  (* Read past any startup output *)
  Lwt_unix.sleep 0.2 >>= fun () ->
  Lwt.return t

let send_command t cmd =
  let wrapped = Printf.sprintf
    "tryCatch({ %s; .rocaml_output_end() }, error = function(e) { message(e$message); .rocaml_error_end() })\n"
    cmd
  in
  Lwt_io.write t.process#stdin wrapped >>= fun () ->
  Lwt_io.flush t.process#stdin

let read_until_sentinel t sentinel =
  let rec loop acc =
    Lwt_io.read_line_opt t.process#stdout >>= function
    | None -> Lwt.return (String.concat "\n" (List.rev acc))
    | Some line ->
      if String.trim line = sentinel then
        Lwt.return (String.concat "\n" (List.rev acc))
      else
        loop (line :: acc)
  in
  loop []

let eval t expr =
  send_command t expr >>= fun () ->
  read_until_sentinel t output_sentinel >>= fun output ->
  let trimmed = String.trim output in
  if trimmed = "" then
    Lwt.return (Output "")
  else
    Lwt.return (Output trimmed)

let complete t ~prefix =
  (* Use R's built-in completion *)
  let cmd = Printf.sprintf
    {|utils:::.getCompletions("%s", %d)|}
    (String.escaped prefix)
    (String.length prefix)
  in
  eval t cmd >>= function
  | Output s ->
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
  | Error _ | Incomplete -> Lwt.return []

let is_complete t expr =
  (* Use R's parser to check if expression is complete *)
  let cmd = Printf.sprintf
    {|tryCatch(parse(text = "%s"), error = function(e) if (grepl("unexpected end", e$message)) "incomplete" else "error")|}
    (String.escaped expr)
  in
  eval t cmd >>= function
  | Output s -> Lwt.return (not (String.equal (String.trim s) {|[1] "incomplete"|}))
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
