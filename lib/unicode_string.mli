(** Unicode-aware string with grapheme cluster support.

    Provides O(1) access to grapheme clusters and display width calculations,
    suitable for terminal text editing. *)

type t
type error = Invalid_utf8 | Leading_combiner

(** {1 Construction} *)

val empty : t
val of_string : string -> (t, error) result
val to_string : t -> string

(** {1 Properties} *)

val length : t -> int
(** Number of grapheme clusters *)

val display_width : t -> int
(** Total display columns *)

val byte_length : t -> int
(** Underlying UTF-8 byte length *)

val is_empty : t -> bool

(** {1 Access} *)

val cluster_at : t -> int -> string
(** UTF-8 bytes of cluster at index *)

val width_at : t -> int -> int
(** Display width of cluster at index *)

val byte_offset_at : t -> grapheme_index:int -> int
(** Byte offset where the grapheme at [grapheme_index] starts. Passing the
    length returns the byte length. *)

val byte_range_at : t -> int -> int * int
(** Byte range occupied by the grapheme at index. *)

val grapheme_index_at_byte : t -> byte_offset:int -> int option
(** Return the grapheme index starting at a byte offset, if the byte offset is
    on a grapheme boundary. The byte length maps to [length t]. *)

val slice_bytes : t -> start_byte:int -> end_byte:int -> string
(** Slice UTF-8 bytes by byte offsets. *)

val prefix_width : t -> int -> int
(** Display width of first n graphemes *)

val truncate_to_display_width : t -> width:int -> t
(** Truncate to display width *)

val grapheme_at_width : t -> int -> int
(** Grapheme index at or before display column. Returns the grapheme index whose
    display position is at or just before the given column. *)

(** {1 Modification} *)

val insert_string : t -> pos:int -> string -> (t, error) result

val delete : t -> int -> t
(** Delete cluster at index *)

val delete_range : t -> start:int -> len:int -> t
val sub : t -> start:int -> len:int -> t

val split : t -> int -> t * t
(** Split at grapheme index, returns (before, after) *)

(** {1 Wrapping} *)

val wrap : t -> width:int -> t list
(** Wrap by display width, returns list of lines *)

(** {1 Convenience} *)

val append : t -> t -> t
val concat : t list -> t
