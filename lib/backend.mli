type t

type response_chunk =
  | Partial of string
  | Complete of string
  | Error of string

type completion = string

val create : unit -> t

(** Send input to the backend. Returns immediately. *)
val submit : t -> string -> unit

(** Block until next response chunk is available. *)
val await_response : t -> response_chunk

(** Cancel the current request. *)
val cancel : t -> unit

(** Block until completions are available. Race this with input. *)
val get_completions : t -> string -> cursor_pos:int -> completion list

val deinit : t -> unit
