type state = {
  path : string;
}

let state : state option ref = ref None

let timestamp () =
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let level_of_string = function
  | "debug" -> Logs.Debug
  | "info" -> Logs.Info
  | "warning" | "warn" -> Logs.Warning
  | "error" -> Logs.Error
  | "app" -> Logs.App
  | _ -> Logs.Info

let configured_level () =
  match Sys.getenv_opt "RAOUI_LOG_LEVEL" with
  | Some level -> level |> String.lowercase_ascii |> level_of_string
  | None -> Logs.Info

let rec ensure_dir dir =
  if dir = "" || dir = "." || dir = "/" then ()
  else if Sys.file_exists dir then begin
    if not (Sys.is_directory dir) then
      invalid_arg (Printf.sprintf "log path parent is not a directory: %s" dir)
  end else begin
    ensure_dir (Filename.dirname dir);
    Unix.mkdir dir 0o700
  end

let default_log_path () =
  match Sys.getenv_opt "RAOUI_LOG_FILE" with
  | Some path when path <> "" -> path
  | _ -> (Paths.resolve ()).log_file

let string_of_level = function
  | Logs.App -> "APP"
  | Logs.Error -> "ERROR"
  | Logs.Warning -> "WARN"
  | Logs.Info -> "INFO"
  | Logs.Debug -> "DEBUG"

let reporter channel =
  let report src level ~over k msgf =
    let finish () =
      flush channel;
      over ();
      k ()
    in
    msgf @@ fun ?header:_ ?tags:_ fmt ->
    Format.kasprintf
      (fun msg ->
        Printf.fprintf channel "%s [%s] %s: %s\n%!"
          (timestamp ()) (string_of_level level) (Logs.Src.name src) msg;
        finish ())
      fmt
  in
  { Logs.report }

let path () = Option.map (fun s -> s.path) !state

let log_exception ?(context = "uncaught exception") exn =
  let bt = Printexc.get_raw_backtrace () |> Printexc.raw_backtrace_to_string in
  if bt = "" then
    Logs.err (fun m -> m "%s: %s" context (Printexc.to_string exn))
  else
    Logs.err (fun m -> m "%s: %s\n%s" context (Printexc.to_string exn) bt)

let init () =
  match !state with
  | Some s -> s.path
  | None ->
      let path = default_log_path () in
      ensure_dir (Filename.dirname path);
      let channel =
        open_out_gen [ Open_creat; Open_text; Open_append ] 0o600 path
      in
      state := Some { path };
      Logs.set_level (Some (configured_level ()));
      Logs.set_reporter (reporter channel);
      Printf.fprintf channel "\n=== raoui session start %s pid=%d ===\n%!"
        (timestamp ()) (Unix.getpid ());
      at_exit (fun () ->
        Logs.app (fun m -> m "session end");
        close_out_noerr channel);
      path
