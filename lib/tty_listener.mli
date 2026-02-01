type key =
  | Char of char
  | Ctrl of char
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Delete
  | Backspace
  | Tab
  | Enter
  | Escape
  | Paste of string
  | Other of string
  | Unknown of string

val await_input : clock:_ Eio.Time.clock -> stdin:_ Eio.Flow.source -> key
