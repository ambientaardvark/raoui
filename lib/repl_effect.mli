type t =
  | Submit of string
  | Cancel
  | RequestCompletions of string * int
  | SubmitReadlineInput of string
  | BackgroundSubmit of string
  | EnterPassthrough
  | Quit
