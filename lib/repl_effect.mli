type backslash_effect =
  | Pick_file of {
      token_start : int;
      original_token : string;
    }

type t =
  | Submit of string
  | Cancel
  | RequestCompletions of string * int
  | SubmitReadlineInput of string
  | BackgroundSubmit of string
  | Run_backslash_effect of backslash_effect
  | EnterPassthrough
  | Quit
