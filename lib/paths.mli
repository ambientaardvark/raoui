type t = {
  config_dir : string;
  state_dir : string;
  cache_dir : string;
  options_file : string;
  history_file : string;
  log_file : string;
  plots_dir : string;
}

val resolve : unit -> t
val ensure_dir : string -> unit
val ensure_runtime_dirs : t -> unit
val export_env : t -> unit
