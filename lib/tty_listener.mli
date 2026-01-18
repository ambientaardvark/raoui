type key =
  | Char of char
  | Ctrl of char
  | Up | Down | Left | Right
  | Home | End
  | Delete | Backspace
  | Tab | Enter | Escape
  | Paste of string
  | Unknown of string

val await_input : unit -> key
