open Frontend_types
open Text_editor

let submit model =
  let text = Unicode_string.to_string (current_line model) in
  let rec safe_guard n =
    let g = String.make n '-' in
    let closing = ")" ^ g ^ "\"" in
    let cl = String.length closing and tl = String.length text in
    let rec found i =
      i <= tl - cl && (String.sub text i cl = closing || found (i + 1))
    in
    if tl >= cl && found 0 then safe_guard (n + 1) else g
  in
  let guard = safe_guard 1 in
  (* Run the command under the user's login shell ($SHELL) rather than the
     /bin/sh that base::system would use, so fish syntax applies. Falls back to
     /bin/sh when $SHELL is unset or empty. For fish we pass --no-config: a -c
     invocation re-sources config.fish (prompt, conda, etc.) on every command,
     adding ~0.7s of latency the REPL side-shell doesn't need — PATH and env are
     already inherited from the launching shell. system2 passes the command as a
     single argument, avoiding a second layer of shell quoting. *)
  let r_command =
    Printf.sprintf
      "local({ s <- Sys.getenv(\"SHELL\"); if (!nzchar(s)) s <- \"/bin/sh\"; \
       a <- if (basename(s) == \"fish\") c(\"--no-config\", \"-c\") else \"-c\"; \
       system2(s, c(a, shQuote(r\"%s(%s)%s\"))) })"
      guard text guard
  in
  History.add_to_history ~mode:"shell" model.input.history model.input.lines;
  ( Mode_common.clear_model_for_submit
      { model with input = { model.input with mode = Normal } },
    [ Repl_effect.Submit r_command ] )

let apply_key key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' -> (Mode_common.set_mode_normal_blank model, [])
  | Ctrl 'p' | Up -> (Mode_common.shift_history model ~amount:1, [])
  | Down -> (Mode_common.shift_history model ~amount:(-1), [])
  | Ctrl 'r' -> (History_search.enter model, [])
  | Enter -> submit model
  | _ -> (
      let model =
        match key with
        | Tab | Escape -> model
        | _ -> (
            match model.input.completion with
            | Some cs when Completion.is_in_completion_mode cs ->
                { model with input = { model.input with completion = None } }
            | _ -> model)
      in
      match key with
      | Ctrl 'd' ->
          if is_empty_input model then
            (Mode_common.set_mode_normal_blank model, [])
          else (delete_char_after_cursor model, [])
      | Ctrl 'u' -> (delete_before_cursor model, [])
      | Ctrl 'a' -> (go_to_line_start model, [])
      | Ctrl 'e' -> (go_to_line_end model, [])
      | Other "next word" -> (go_to_next_word model, [])
      | Other "last word" -> (go_to_last_word model, [])
      | Char c -> (insert_char model c, [])
      | Backspace when prompt_is_empty model ->
          (Mode_common.set_mode_normal_blank model, [])
      | Backspace -> (delete_char model, [])
      | Left -> (move_left model, [])
      | Right -> (move_right model, [])
      | Paste text -> (insert_paste model text, [])
      | _ -> (model, []))
