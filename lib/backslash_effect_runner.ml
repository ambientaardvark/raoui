let quote_r_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let run_command_capture command =
  let ic = Unix.open_process_in command in
  let status = ref (Unix.WEXITED 1) in
  let output =
    Fun.protect
      (fun () -> In_channel.input_all ic)
      ~finally:(fun () -> status := Unix.close_process_in ic)
  in
  match !status with
  | Unix.WEXITED 0 ->
      let result = String.trim output in
      if result = "" then None else Some result
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> None

let with_normal_terminal ~orig_termios f =
  Terminal_session.restore_mode orig_termios;
  Terminal_session.disable_bracketed_paste ();
  Fun.protect f ~finally:(fun () ->
      ignore (Terminal_session.set_raw_mode ());
      Terminal_session.enable_bracketed_paste ())

let choose_file ~orig_termios =
  let command = "osascript -e 'POSIX path of (choose file)'" in
  with_normal_terminal ~orig_termios (fun () -> run_command_capture command)

let choose_file_fzf ~orig_termios =
  let command =
    "if command -v fd >/dev/null 2>&1; then fd --type f --strip-cwd-prefix; \
     else find . -type f | sed 's#^\\./##'; fi | fzf"
  in
  with_normal_terminal ~orig_termios (fun () -> run_command_capture command)

let normalize_selected_path path =
  assert (path <> "");
  let normalized =
    if Filename.is_relative path then Unix.realpath path else path
  in
  assert (not (Filename.is_relative normalized));
  normalized

let inserted_path_text selected_path =
  selected_path |> normalize_selected_path |> quote_r_string

let run ~orig_termios = function
  | Repl_effect.Pick_file { token_start; original_token } ->
      let inserted_text =
        choose_file ~orig_termios |> Option.map inserted_path_text
      in
      Update.Backslash_effect_result
        { token_start; original_token; inserted_text }
  | Repl_effect.Pick_file_fzf { token_start; original_token } ->
      let inserted_text =
        choose_file_fzf ~orig_termios |> Option.map inserted_path_text
      in
      Update.Backslash_effect_result
        { token_start; original_token; inserted_text }
