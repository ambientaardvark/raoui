type t = {
  config_dir : string;
  state_dir : string;
  cache_dir : string;
  options_file : string;
  history_file : string;
  log_file : string;
  plots_dir : string;
  plot_session_dir : string;
}

let app_name = "raoui"
let plot_session_age_cutoff_seconds = 48. *. 60. *. 60.

let getenv_nonempty name =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> Some value
  | _ -> None

let home_dir () =
  match getenv_nonempty "HOME" with
  | Some home -> home
  | None -> Sys.getcwd ()

let xdg_dir ~env ~fallback_suffix =
  match getenv_nonempty env with
  | Some dir -> Filename.concat dir app_name
  | None -> Filename.concat (home_dir ()) fallback_suffix

let plot_session_dir_name ~pid ~started_at =
  Printf.sprintf "%d-%.0f" pid started_at

let parse_plot_session_dir_name name =
  match String.split_on_char '-' name with
  | [ pid_s; started_at_s ] -> (
      match int_of_string_opt pid_s, float_of_string_opt started_at_s with
      | Some pid, Some started_at -> Some (pid, started_at)
      | _ -> None)
  | _ -> None

let is_pid_alive pid =
  try
    Unix.kill pid 0;
    true
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | Unix.Unix_error (Unix.EPERM, _, _) -> true
  | Unix.Unix_error _ -> true

let should_remove_plot_session ~now ~pid ~started_at =
  now -. started_at >= plot_session_age_cutoff_seconds && not (is_pid_alive pid)

let rec remove_tree path =
  if Sys.file_exists path then
    let stat = Unix.lstat path in
    if stat.Unix.st_kind = Unix.S_DIR then begin
      Sys.readdir path
      |> Array.iter (fun entry -> remove_tree (Filename.concat path entry));
      Unix.rmdir path
    end else
      Sys.remove path

let resolve () =
  let started_at = Unix.gettimeofday () in
  let config_dir = xdg_dir ~env:"XDG_CONFIG_HOME" ~fallback_suffix:".config/raoui" in
  let state_dir = xdg_dir ~env:"XDG_STATE_HOME" ~fallback_suffix:".local/state/raoui" in
  let cache_dir = xdg_dir ~env:"XDG_CACHE_HOME" ~fallback_suffix:".cache/raoui" in
  let plots_dir = Filename.concat cache_dir "plots" in
  let plot_session_dir =
    Filename.concat plots_dir
      (plot_session_dir_name ~pid:(Unix.getpid ()) ~started_at)
  in
  {
    config_dir;
    state_dir;
    cache_dir;
    options_file = Filename.concat config_dir "config.toml";
    history_file = Filename.concat state_dir "history";
    log_file = Filename.concat state_dir "raoui.log";
    plots_dir;
    plot_session_dir;
  }

let rec ensure_dir dir =
  if dir = "" || dir = "." || dir = "/" then ()
  else if Sys.file_exists dir then begin
    if not (Sys.is_directory dir) then
      invalid_arg (Printf.sprintf "path is not a directory: %s" dir)
  end else begin
    ensure_dir (Filename.dirname dir);
    Unix.mkdir dir 0o700
  end

let ensure_runtime_dirs t =
  ensure_dir t.config_dir;
  ensure_dir t.state_dir;
  ensure_dir t.cache_dir;
  ensure_dir t.plots_dir;
  ensure_dir t.plot_session_dir

let export_one name value = Unix.putenv name value

let export_env t =
  export_one "RAOUI_CONFIG_DIR" t.config_dir;
  export_one "RAOUI_STATE_DIR" t.state_dir;
  export_one "RAOUI_CACHE_DIR" t.cache_dir;
  export_one "RAOUI_OPTIONS_FILE" t.options_file;
  export_one "RAOUI_HISTORY_FILE" t.history_file;
  export_one "RAOUI_LOG_FILE" t.log_file;
  export_one "RAOUI_PLOTS_DIR" t.plot_session_dir

let cleanup_stale_plot_sessions t =
  if Sys.file_exists t.plots_dir && Sys.is_directory t.plots_dir then
    let now = Unix.gettimeofday () in
    Sys.readdir t.plots_dir
    |> Array.iter (fun entry ->
           let full_path = Filename.concat t.plots_dir entry in
           if Sys.file_exists full_path && Sys.is_directory full_path then
             match parse_plot_session_dir_name entry with
             | Some (pid, started_at)
               when should_remove_plot_session ~now ~pid ~started_at ->
                 (try
                    remove_tree full_path;
                    Logs.info (fun m -> m "removed stale plot cache %s" full_path)
                  with exn ->
                    Logs.warn (fun m ->
                        m "failed to remove stale plot cache %s: %s"
                          full_path (Printexc.to_string exn)))
             | _ -> ())
