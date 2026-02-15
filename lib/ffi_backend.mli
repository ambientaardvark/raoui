type t

type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown
  | Restarted of string
  | Passthrough
  | Passthrough_end
  | Completions of string * string list  (* token * items *)

type completion = string

val create : sw:Eio.Switch.t -> unit -> t
val poll_ready : t -> bool
val submit : t -> string -> unit
val background_submit : t -> string -> unit
val await_response : t -> response_chunk
val signal_passthrough : unit -> unit
val cancel : t -> unit
val request_completions : t -> string -> cursor_pos:int -> unit
val restart : t -> unit
val deinit : t -> unit
