type simple_command = {
  name : string;
  inserted_text : string;
}

type effectful_command =
  | Pick_file

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

let default_registry =
  [
    Simple { name = "pi"; inserted_text = "π" };
    Simple { name = "alpha"; inserted_text = "α" };
    Simple { name = "beta"; inserted_text = "β" };
    Simple { name = "gamma"; inserted_text = "γ" };
    Simple { name = "delta"; inserted_text = "δ" };
    Simple { name = "epsilon"; inserted_text = "ε" };
    Simple { name = "theta"; inserted_text = "θ" };
    Simple { name = "lambda"; inserted_text = "λ" };
    Simple { name = "mu"; inserted_text = "μ" };
    Simple { name = "sigma"; inserted_text = "σ" };
    Simple { name = "phi"; inserted_text = "φ" };
    Simple { name = "psi"; inserted_text = "ψ" };
    Simple { name = "omega"; inserted_text = "ω" };
    Effectful { name = "file"; action = Pick_file };
  ]

let name = function
  | Simple { name; _ } -> name
  | Effectful { name; _ } -> name

let label command = "\\" ^ name command

let completion_labels commands = List.map label commands

let find_by_label registry needle =
  List.find_opt (fun command -> String.equal (label command) needle) registry

let is_command_char s =
  String.length s = 1
  &&
  match String.get s 0 with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let is_token_boundary s = not (String.equal s "\\" || is_command_char s)

(* Scans leftward from (cursor_pos - 1), consuming command chars until a
   backslash is found.  Returns Some (backslash_idx, typed_text) when the
   sequence looks like a backslash command, or None otherwise. *)
let scan_backslash_token line ~cursor_pos =
  if cursor_pos <= 0 || cursor_pos > Unicode_string.length line then None
  else
    let rec consume_name idx =
      if idx < 0 then -1
      else
        let cluster = Unicode_string.cluster_at line idx in
        if is_command_char cluster then consume_name (idx - 1) else idx
    in
    let backslash_idx = consume_name (cursor_pos - 1) in
    if backslash_idx < 0 then None
    else if not (String.equal (Unicode_string.cluster_at line backslash_idx) "\\")
    then None
    else
      let valid_boundary =
        backslash_idx = 0
        || is_token_boundary (Unicode_string.cluster_at line (backslash_idx - 1))
      in
      if not valid_boundary then None
      else
        let typed_len = cursor_pos - backslash_idx in
        let typed_text =
          Unicode_string.sub line ~start:backslash_idx ~len:typed_len
          |> Unicode_string.to_string
        in
        if String.length typed_text <= 1 || String.equal typed_text "\\\\"
        then None
        else Some (backslash_idx, typed_text)

let token_in_line line ~cursor_pos =
  scan_backslash_token line ~cursor_pos
  |> Option.map (fun (backslash_idx, typed_text) ->
         let command_name_prefix =
           String.sub typed_text 1 (String.length typed_text - 1)
         in
         { token_start = backslash_idx; typed_text; command_name_prefix })

let exact_command_before_cursor registry line ~cursor_pos =
  Option.bind (scan_backslash_token line ~cursor_pos)
    (fun (backslash_idx, typed_text) ->
       find_by_label registry typed_text
       |> Option.map (fun command -> (backslash_idx, command)))

let matching_commands registry ~prefix =
  List.filter
    (fun command ->
      let name = name command in
      String.length name >= String.length prefix
      && String.sub name 0 (String.length prefix) = prefix)
    registry
