open Frontend_types
open Text_editor

type msg =
  | Key of Tty_listener.key
  | Response of Ffi_backend.response_chunk
  | Backslash_effect_result of {
      token_start : int;
      original_token : string;
      inserted_text : string option;
    }
  | TermResize of int * int

let handle_vertical_cursor_movement model =
  let width = effective_width model in
  (* Completions are rendered as an overlay in view.ml and should not
     contribute to prompt box height or scrolling math. *)
  let dropdown_rows = 0 in
  let new_height =
    model.lines |> wrap_lines width |> List.length |> ( + ) dropdown_rows
    |> max model.prompt_box_height
    |> max min_prompt_height
  in
  let scrolls_from_expansion =
    if new_height > model.prompt_box_height then
      let bottom = model.prompt_top_row + new_height - 1 in
      if bottom > model.term_height then model.term_height - bottom else 0
    else 0
  in
  let prompt_top_after_expansion =
    model.prompt_top_row + scrolls_from_expansion
  in
  let cursor_term_row = model.cursor_row + prompt_top_after_expansion in
  let scrolls_from_cursor_movement =
    if cursor_term_row > model.term_height then
      model.term_height - cursor_term_row
    else if cursor_term_row < 1 then 1 - cursor_term_row
    else 0
  in
  {
    model with
    prompt_box_height = new_height;
    prompt_top_row =
      model.prompt_top_row + scrolls_from_expansion
      + scrolls_from_cursor_movement;
    scroll_amount = scrolls_from_expansion;
  }

let handle_resize new_width new_height model =
  if model.term_width = new_width && model.term_height = new_height then model
  else
    let model =
      { model with term_width = new_width; term_height = new_height }
    in
    let new_eff_width = effective_width model in
    let new_row, new_col =
      internal_to_terminal new_eff_width model.lines
        (model.cursor_line, model.cursor_pos)
    in
    let prompt_top = clamp_prompt_top new_height model.prompt_top_row in
    {
      model with
      cursor_row = new_row;
      cursor_col = new_col;
      prompt_top_row = prompt_top;
      prompt_box_height = min_prompt_height;
    }

let grapheme_cluster_count text =
  match Unicode_string.of_string text with
  | Ok u -> Ok (Unicode_string.length u)
  | Error e -> Error e

let submit model =
  match model.mode with
  | Readline _ -> Readline_mode.submit model
  | Shell -> Shell_mode.submit model
  | Normal -> Normal_mode.submit model
  | History_search _ -> ({ model with mode = Normal }, [])

(* Key handler router *)
let apply_key key model =
  match model.mode with
  | Readline _ -> Readline_mode.apply_key key model
  | Shell -> Shell_mode.apply_key key model
  | Normal -> Normal_mode.apply_key key model
  | History_search _ -> History_search.apply_key key model

let universal_corrections key model =
  model
  |> Completion_controller.filter_completions
  |> handle_vertical_cursor_movement
  |> fun s ->
  let flipping_through_history =
    match model.flipping_through_history with
    | Some 1 | None -> None
    | Some n -> Some (n - 1)
  in
  { s with previous_key = Some key; flipping_through_history }

let sync_internal_coords model =
  let width = effective_width model in
  let cursor_line, cursor_pos =
    terminal_to_internal width model.lines (model.cursor_row, model.cursor_col)
  in
  { model with cursor_line; cursor_pos }

let handle_key_input key model =
  let sync model =
    match model.mode with
    | History_search _ -> model
    | _ -> sync_internal_coords model
  in
  let new_model, effects =
    model
    |> handle_resize model.term_width model.term_height
    |> sync
    |> apply_key key
  in
  (universal_corrections key new_model, effects)


let process_response model =
  match model.backend_response with
  | None -> failwith "process_response called with no backend_response"
  | Some response ->
      let repl_output =
        match response with
        | Ffi_backend.Stdout s -> Output_text [ (`Raw, s) ]
        | Ffi_backend.Result s -> Output_text [ (`Raw, s) ]
        | Ffi_backend.R_error s -> Output_text [ (`Error, s) ]
        | Ffi_backend.Internal_error s ->
            Output_text [ (`Error, "Internal error: " ^ s) ]
        | Ffi_backend.Restarted s -> Output_text [ (`Error, s) ]
        | Ffi_backend.Image image -> Output_image image
        | Ffi_backend.Done -> Output_text []
        | Ffi_backend.Shutdown -> Output_text []
        | Ffi_backend.Passthrough | Ffi_backend.Passthrough_end
        | Ffi_backend.Completions _ | Ffi_backend.Readline _ ->
            Output_text []
      in
      let awaiting_response =
        match response with
        (* Keep waiting for more output until we get a terminal response *)
        | Ffi_backend.Stdout _ | Ffi_backend.Result _ | Ffi_backend.R_error _
        | Ffi_backend.Readline _ | Ffi_backend.Image _ ->
            model.awaiting_response
        (* Terminal responses *)
        | Ffi_backend.Done | Ffi_backend.Shutdown | Ffi_backend.Internal_error _
        | Ffi_backend.Restarted _ | Ffi_backend.Passthrough
        | Ffi_backend.Passthrough_end | Ffi_backend.Completions _ ->
            false
      in
      let mode =
        match response with
        | Ffi_backend.Readline prompt ->
            let normalized_prompt = if prompt = "" then "input" else prompt in
            Frontend_types.Readline normalized_prompt
        | Ffi_backend.Done -> Frontend_types.Normal (* Reset on completion *)
        | _ -> model.mode (* Preserve current mode *)
      in
      {
        model with
        backend_response = None;
        repl_output = Some repl_output;
        awaiting_response;
        scroll_amount = 0;
        mode;
      }

let update msg model =
  let model = { model with scroll_amount = 0 } in
  match msg with
  | Key key -> (
      let m, effects = handle_key_input key model in
      match effects with
      | [] -> Completion_controller.completion_followup m
      | _ -> (m, effects))
  | Response response -> (
      match response with
      | Ffi_backend.Shutdown -> (model, [ Repl_effect.Quit ])
      | Ffi_backend.Passthrough -> (model, [ Repl_effect.EnterPassthrough ])
      | Ffi_backend.Passthrough_end -> (model, [])
      | Ffi_backend.Completions (token, items) -> (
          match model.mode with
          | History_search _ -> ({ model with completion = None }, [])
          | _ ->
              let in_completion_mode =
                match model.completion with
                | Some cs when Completion.is_in_completion_mode cs -> true
                | _ -> false
              in
              if in_completion_mode then (model, [])
              else
                let token_start = model.cursor_pos - String.length token in
                let completion =
                  Completion.create ~token_start
                    (List.map Completion.backend_item items)
                in
                let m = { model with completion = Some completion } in
                (Completion_controller.filter_completions m, []))
      | Ffi_backend.Restarted _ ->
          let m =
            { model with backend_response = Some response }
            |> process_response |> handle_vertical_cursor_movement
          in
          ( m,
            [
              Repl_effect.BackgroundSubmit
                (Printf.sprintf "options(width=%d)" model.term_width);
            ] )
      | _ ->
          let m =
            { model with backend_response = Some response }
            |> process_response |> handle_vertical_cursor_movement
          in
          (m, []))
  | Backslash_effect_result { token_start; original_token; inserted_text } ->
      let original_len =
        grapheme_cluster_count original_token |> Result.get_ok
      in
      let token_end = token_start + original_len in
      let m =
        match inserted_text with
        | None -> { model with completion = None }
        | Some text ->
            let replaced = replace_range model token_start token_end text in
            { replaced with completion = None }
      in
      (m, [])
  | TermResize (width, height) ->
      let m =
        handle_resize width height model |> handle_vertical_cursor_movement
      in
      ( m,
        [
          Repl_effect.BackgroundSubmit
            (Printf.sprintf "options(width=%d)" width);
        ] )
