type t

type image = {
  source_path : string;
  preview_path : string;
  mime_type : string option;
  width_px : int option;
  height_px : int option;
}

type response_chunk =
  | Stdout of string
  | Result of string
  | R_error of string
  | Internal_error of string
  | Done
  | Shutdown
  | Restarted of string
  | Image of image
  | Passthrough
  | Passthrough_end
  | Completions of string * string list  (* token * items *)
  | Readline of string  (* prompt *)

type completion = string

val create : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> unit -> t
val poll_ready : t -> bool
val submit : t -> string -> unit
val background_submit : t -> string -> unit
val await_response : t -> response_chunk
val signal_passthrough : unit -> unit
val submit_readline_input : string -> unit
val cancel : t -> unit
val request_completions : t -> string -> cursor_pos:int -> unit
val restart : t -> unit
val deinit : t -> unit

(** Exposed for testing. *)
val parse_image_payload : string -> image option
