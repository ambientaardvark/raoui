type key =
  | Char of string
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

val await_input_with_timeout :
  escape_timeout_sec:float ->
  clock:_ Eio.Time.clock ->
  stdin:_ Eio.Flow.source ->
  key

val drain_to_keys : clock:_ Eio.Time.clock -> stdin:_ Eio.Flow.source -> key list

val drain_to_keys_with_timeouts :
  escape_timeout_sec:float ->
  settle_timeout_sec:float ->
  clock:_ Eio.Time.clock ->
  stdin:_ Eio.Flow.source ->
  key list
