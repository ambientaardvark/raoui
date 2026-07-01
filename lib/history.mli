type t

type output = {
  kind : string;
  text : string option;
  image_path : string option;
}

type interaction = {
  input : string;
  mode : string;
  submitted_at : float;
  outputs : output list;
}

val init : string -> t
val close : t -> unit

val add_to_history : ?mode:string -> t -> Unicode_string.t list -> unit
(** to call after when submitting. Adds the command to the history *)

val record_response : t -> Ffi_backend.response_chunk -> unit
(** Records visible output for the active command, if any. *)

val record_cancel : t -> unit
(** Marks the active command interrupted, if any. *)

val go_back :
  t ->
  ?current_prompt:string * Unicode_string.t list ->
  unit ->
  (string * Unicode_string.t list) option
(** older entry as [(mode, lines)] where mode is "r", "shell", or "ai".
    [current_prompt] is the in-progress [(mode, lines)] draft, saved so
    navigating forwards past the newest entry restores it. None at end. *)

val go_forwards :
  t ->
  ?current_prompt:string * Unicode_string.t list ->
  unit ->
  (string * Unicode_string.t list) option
(** newer entry as [(mode, lines)]. if at start return None *)

val get_all : t -> string array

val search_matches : t -> string -> limit:int -> (string * string) list
(** Up to [limit] distinct entries whose text contains the needle
    (case-insensitive substring; an empty needle matches everything),
    newest-first, as [(mode, text)]. *)

val recent_interactions : t -> limit:int -> interaction list

val search_interactions : t -> keyword:string -> limit:int -> interaction list
(** Interactions in this session whose input or output text contains [keyword]. *)
