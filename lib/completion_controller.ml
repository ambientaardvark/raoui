open Frontend_types
open Text_editor

let filter_completions model =
  match model.mode with
  | History_search _ -> { model with completion = None }
  | _ -> (
      match model.completion with
      | None -> model
      | Some cs when Completion.is_in_completion_mode cs ->
          model
      | Some cs -> (
          let line = current_line model in
          let line_len = Unicode_string.length line in
          let token_start = Completion.token_start cs in
          if token_start > line_len || token_start > model.cursor_pos then
            { model with completion = None }
          else
            let prefix_len = model.cursor_pos - token_start in
            let prefix =
              Unicode_string.to_string
                (Unicode_string.sub line ~start:token_start ~len:prefix_len)
            in
            match Completion.filter cs ~prefix with
            | None -> { model with completion = None }
            | Some filtered -> { model with completion = Some filtered }))

let handle_tab model =
  match model.completion with
  | None -> model
  | Some cs -> (
      if List.length (Completion.filtered_items cs) = 0 then model
      else
        let cs_with_original =
          if not (Completion.is_in_completion_mode cs) then
            let line = current_line model
            and token_start = Completion.token_start cs in
            let prefix_len = model.cursor_pos - token_start in
            let original =
              Unicode_string.to_string
                (Unicode_string.sub line ~start:token_start ~len:prefix_len)
            in
            Completion.save_original_token cs ~token:original
          else cs
        in
        let cs_cycled = Completion.cycle_next cs_with_original in
        match Completion.current_completion cs_cycled with
        | None -> model
        | Some completion_item ->
            let token_start = Completion.token_start cs_cycled in
            let inserted =
              replace_token model token_start (Completion.label completion_item)
            in
            { inserted with completion = Some cs_cycled })

let accept_backslash_completion model =
  let apply_command token_start label command =
    match command with
    | Backslash_command.Simple { inserted_text; _ } ->
        let replaced =
          replace_range model token_start model.cursor_pos inserted_text
        in
        Some ({ replaced with completion = None }, [])
    | Backslash_command.Effectful
        { action = Backslash_command.Pick_file; _ } ->
        Some
          ( { model with completion = None },
            [
              Repl_effect.Run_backslash_effect
                (Repl_effect.Pick_file
                   { token_start; original_token = label });
            ] )
  in
  let fallback_exact_command () =
    let line = current_line model in
    match
      Backslash_command.exact_command_before_cursor
        Backslash_command.default_registry line ~cursor_pos:model.cursor_pos
    with
    | None -> None
    | Some (token_start, command) ->
        apply_command token_start
          (Backslash_command.label command)
          command
  in
  match model.completion with
  | None -> fallback_exact_command ()
  | Some cs -> (
      let exact_match_item () =
        let line = current_line model in
        let token_start = Completion.token_start cs in
        if token_start > model.cursor_pos then None
        else
          let typed_len = model.cursor_pos - token_start in
          let typed_text =
            Unicode_string.sub line ~start:token_start ~len:typed_len
            |> Unicode_string.to_string
          in
          Completion.filtered_items cs
          |> List.find_opt (fun item ->
              String.equal (Completion.label item) typed_text)
      in
      let chosen_item =
        match Completion.current_completion cs with
        | Some item -> Some item
        | None -> exact_match_item ()
      in
      match chosen_item with
      | Some completion_item -> (
          match Completion.kind completion_item with
          | Completion.Backslash command ->
              let token_start = Completion.token_start cs in
              apply_command token_start (Completion.label completion_item) command
          | Completion.Backend -> None)
      | None -> fallback_exact_command ())

let make_backslash_completion model (token : Backslash_command.token) =
  let items =
    Backslash_command.matching_commands Backslash_command.default_registry
      ~prefix:token.command_name_prefix
    |> List.map Completion.backslash_item
  in
  if items = [] then { model with completion = None }
  else
    let completion = Completion.create ~token_start:token.token_start items in
    filter_completions { model with completion = Some completion }

let completion_followup model =
  match model.mode with
  | History_search _ -> ({ model with completion = None }, [])
  | _ ->
      let normal_completion_token_length line =
        let rec loop idx len =
          if idx < 0 then len
          else
            let cluster = Unicode_string.cluster_at line idx in
            if is_word_char cluster then loop (idx - 1) (len + 1) else len
        in
        loop (model.cursor_pos - 1) 0
      in
      let in_completion_mode =
        match model.completion with
        | Some cs when Completion.is_in_completion_mode cs -> true
        | _ -> false
      in
      if (not model.awaiting_response) && not in_completion_mode then
        match List.nth_opt model.lines model.cursor_line with
        | Some line -> (
            match
              Backslash_command.token_in_line line ~cursor_pos:model.cursor_pos
            with
            | Some token -> (make_backslash_completion model token, [])
            | None when normal_completion_token_length line < 2 ->
                ({ model with completion = None }, [])
            | None ->
                let text = Unicode_string.to_string line in
                [ Repl_effect.RequestCompletions (text, model.cursor_pos) ]
                |> fun effects -> (model, effects))
        | None -> (model, [])
      else (model, [])
