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

val resolve : unit -> t
val ensure_dir : string -> unit
val ensure_runtime_dirs : t -> unit
val export_env : t -> unit
val cleanup_stale_plot_sessions : t -> unit

(** Exposed for testing. *)
val plot_session_dir_name : pid:int -> started_at:float -> string

(** Exposed for testing. *)
val parse_plot_session_dir_name : string -> (int * float) option

(** Exposed for testing. *)
val should_remove_plot_session :
  now:float -> pid:int -> started_at:float -> bool
