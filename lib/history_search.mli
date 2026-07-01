open Frontend_types

val fresh_state : History.t -> Unicode_string.t -> search_state
(** Pager state for a query: its matches (newest-first; an empty query matches
    everything) with the newest selected. *)

val enter : model -> model
(** Switch to history-search mode with an empty query, from any input mode. *)

val apply_key : Tty_listener.key -> model -> model * Repl_effect.t list
