type backslash_effect =
  | Pick_file of {
      token_start : int;
      original_token : string;
    }
  | Pick_file_fzf of {
      token_start : int;
      original_token : string;
    }

type t =
  | Submit of string
  | Cancel
  | RequestCompletions of string * int
  | SubmitReadlineInput of string
  | BackgroundSubmit of string
  | SubmitAiQuery of string
  | ResetAiSession  (* start a fresh AI conversation (new claude session) *)
  | Run_backslash_effect of backslash_effect
  | EnterPassthrough
  | Quit
