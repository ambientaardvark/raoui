type simple_command = {
  name : string;
  inserted_text : string;
}

type effectful_command =
  | Pick_file
  | Pick_file_fzf

type command =
  | Simple of simple_command
  | Effectful of {
      name : string;
      action : effectful_command;
    }

type registry = command list

type token = {
  token_start : int;
  typed_text : string;
  command_name_prefix : string;
}

val default_registry : registry
val token_in_line : Unicode_string.t -> cursor_pos:int -> token option
val exact_command_before_cursor :
  registry -> Unicode_string.t -> cursor_pos:int -> (int * command) option
val matching_commands : registry -> prefix:string -> command list
val completion_labels : command list -> string list
val find_by_label : registry -> string -> command option
val name : command -> string
val label : command -> string
