(** Completion state management *)

(** Opaque completion state *)
type t

type item_kind =
  | Backend
  | Backslash of Backslash_command.command

type item

(** Maximum number of visible completion rows in the dropdown *)
val max_visible_rows : int

(** Create initial completion state from backend results *)
val create : token_start:int -> item list -> t

val backend_item : string -> item
val backslash_item : Backslash_command.command -> item
val label : item -> string
val kind : item -> item_kind

(** Filter completions by prefix. Returns None if no matches. *)
val filter : t -> prefix:string -> t option

(** Cycle to next completion (wraps around) *)
val cycle_next : t -> t

(** Get the currently selected completion, if any *)
val current_completion : t -> item option

(** Check if user is in completion mode (actively cycling with Tab) *)
val is_in_completion_mode : t -> bool

(** Get the token start position *)
val token_start : t -> int

(** Save the original token text (for Escape revert) *)
val save_original_token : t -> token:string -> t

(** Get the saved original token *)
val original_token : t -> string

(** Get number of rows needed for dropdown (max 4) *)
val dropdown_size : t -> int

(** Get all filtered completions (for display) *)
val filtered_items : t -> item list

(** Get the start index of the visible completion window *)
val visible_window_start : t -> int

(** Get the visible completion items for the dropdown *)
val visible_items : t -> item list

(** Get the selected index within the visible window, if any *)
val selected_index_in_window : t -> int option

(** Get selected index (-1 means dropdown only, 0+ means completion mode) *)
val selected_index : t -> int option
