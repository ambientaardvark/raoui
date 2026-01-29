(** Unicode-aware string with grapheme cluster support.

    Provides O(1) access to grapheme clusters and display width calculations,
    suitable for terminal text editing. *)

type t

type error =
  | Invalid_utf8
  | Leading_combiner

(** {1 Construction} *)

val empty : t
val of_string : string -> (t, error) result
val to_string : t -> string

(** {1 Properties} *)

(** Number of grapheme clusters *)
val length : t -> int

(** Total display columns *)
val display_width : t -> int

(** Underlying UTF-8 byte length *)
val byte_length : t -> int

val is_empty : t -> bool

(** {1 Access} *)

(** UTF-8 bytes of cluster at index *)
val cluster_at : t -> int -> string

(** Display width of cluster at index *)
val width_at : t -> int -> int

(** Display width of first n graphemes *)
val prefix_width : t -> int -> int

(** Grapheme index at or before display column. Returns the grapheme index
    whose display position is at or just before the given column. *)
val grapheme_at_width : t -> int -> int

(** {1 Modification} *)

val insert_string : t -> pos:int -> string -> (t, error) result

(** Delete cluster at index *)
val delete : t -> int -> t

val delete_range : t -> start:int -> len:int -> t
val sub : t -> start:int -> len:int -> t

(** Split at grapheme index, returns (before, after) *)
val split : t -> int -> t * t

(** {1 Wrapping} *)

(** Wrap by display width, returns list of lines *)
val wrap : t -> width:int -> t list

(** {1 Convenience} *)

val append : t -> t -> t
val concat : t list -> t
