type t =
  | Submit of string
  | Cancel
  | RequestCompletions of string * int
  | SubmitReadlineInput of string
  | Quit
