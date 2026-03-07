open Frontend_types

val apply_key : Tty_listener.key -> model -> model * Repl_effect.t list
