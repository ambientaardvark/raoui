type t

val init : string -> t
val close : t -> unit

val add_to_history : t -> Unicode_string.t list -> unit
(** to call after when submitting. Adds the command to the history *)

val go_back :
  t ->
  ?current_prompt:Unicode_string.t list ->
  unit ->
  Unicode_string.t list option
(** older message. if at end return None *)

val go_forwards :
  t ->
  ?current_prompt:Unicode_string.t list ->
  unit ->
  Unicode_string.t list option
(** newer message. if at start return None *)

val get_all : t -> string array
val search_history : t -> string -> string
