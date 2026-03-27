type t = {
  config_dir : string;
  state_dir : string;
  cache_dir : string;
  options_file : string;
  history_file : string;
  log_file : string;
  plots_dir : string;
}

let app_name = "raoui"

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

let resolve () =
  let config_dir = xdg_dir ~env:"XDG_CONFIG_HOME" ~fallback_suffix:".config/raoui" in
  let state_dir = xdg_dir ~env:"XDG_STATE_HOME" ~fallback_suffix:".local/state/raoui" in
  let cache_dir = xdg_dir ~env:"XDG_CACHE_HOME" ~fallback_suffix:".cache/raoui" in
  {
    config_dir;
    state_dir;
    cache_dir;
    options_file = Filename.concat config_dir "config.toml";
    history_file = Filename.concat state_dir "history";
    log_file = Filename.concat state_dir "raoui.log";
    plots_dir = Filename.concat cache_dir "plots";
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
  ensure_dir t.plots_dir

let export_one name value = Unix.putenv name value

let export_env t =
  export_one "RAOUI_CONFIG_DIR" t.config_dir;
  export_one "RAOUI_STATE_DIR" t.state_dir;
  export_one "RAOUI_CACHE_DIR" t.cache_dir;
  export_one "RAOUI_OPTIONS_FILE" t.options_file;
  export_one "RAOUI_HISTORY_FILE" t.history_file;
  export_one "RAOUI_LOG_FILE" t.log_file;
  export_one "RAOUI_PLOTS_DIR" t.plots_dir
