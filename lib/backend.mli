type t

type response_chunk =
  | Stdout of string         (** stream/stdout - print(), cat(), message() *)
  | Result of string         (** execute_result - the return value *)
  | R_error of string        (** R code error - stop(), undefined var, syntax error *)
  | Internal_error of string (** OCaml or ark kernel failure *)
  | Done                     (** reached idle - terminal *)
  | Shutdown                 (** SIGINT - terminal *)
  | Restarted of string      (** backend restarted after disconnect *)

type completion = string

val create : unit -> t

(** Non-blocking check if kernel is ready. Returns true once the initial
    startup idle has been received. Call in a loop with Fiber.yield. *)
val poll_ready : t -> bool

(** Send input to the backend. Returns immediately. *)
val submit : t -> string -> unit

(** Send code to the backend which should be executed in the background *)
val background_submit: t -> string -> unit

(** Block until next response chunk is available.
    Caller should keep calling until receiving Done, Shutdown, or Internal_error.
    R_error is NOT terminal - more chunks may follow (and Done will follow). *)
val await_response : t -> response_chunk

(** Cancel the current request. *)
val cancel : t -> unit

(** Block until completions are available. Race this with input. *)
val get_completions : t -> string -> cursor_pos:int -> completion list

val restart : t -> unit

val deinit : t -> unit

(* Exposed for testing *)
val random_hex_token : int -> string
val sign : string list -> string -> string
