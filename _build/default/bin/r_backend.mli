(** R backend interface.

    This module defines the interface for communicating with R.
    The initial implementation uses a subprocess, but this can be
    swapped out for an embedded R runtime via FFI later. *)

(** Result of evaluating an R expression *)
type eval_result =
  | Output of string
  | Error of string
  | Incomplete  (* expression needs more input *)

(** An opaque R value handle - for future FFI use *)
type r_value

(** The backend interface *)
module type S = sig
  type t

  val start : unit -> t Lwt.t
  (** Start the R backend *)

  val eval : t -> string -> eval_result Lwt.t
  (** Evaluate an R expression and return the result *)

  val complete : t -> prefix:string -> string list Lwt.t
  (** Get completions for a prefix *)

  val is_complete : t -> string -> bool Lwt.t
  (** Check if an expression is syntactically complete *)

  val shutdown : t -> unit Lwt.t
  (** Shutdown the R backend *)

  (* Future expansion for direct memory access *)
  val get_value : t -> string -> r_value option Lwt.t
  (** Get an R value by name (for future FFI backends) *)
end
