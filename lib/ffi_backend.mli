type t

type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown
  | Restarted of string

type completion = string

val create : sw:Eio.Switch.t -> unit -> t
val poll_ready : t -> bool
val submit : t -> string -> unit
val background_submit : t -> string -> unit
val await_response : t -> response_chunk
val cancel : t -> unit
val get_completions : t -> string -> cursor_pos:int -> completion list
val restart : t -> unit
val deinit : t -> unit
