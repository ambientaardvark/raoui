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
  let content_height =
    model.input.lines |> wrap_lines width |> List.length |> ( + ) dropdown_rows
  in
  let new_height =
    content_height
    |> max model.layout.prompt_box_height
    |> max min_prompt_height
  in
  let scrolls_from_expansion =
    if new_height > model.layout.prompt_box_height then
      let bottom = model.layout.prompt_top_row + new_height - 1 in
      if bottom > model.layout.term_height then
        model.layout.term_height - bottom
      else 0
    else 0
  in
  let prompt_top_after_expansion =
    model.layout.prompt_top_row + scrolls_from_expansion
  in
  let cursor_row, _ = cursor_terminal_pos model in
  let cursor_term_row = cursor_row + prompt_top_after_expansion in
  let scrolls_from_cursor_movement =
    if cursor_term_row > model.layout.term_height then
      model.layout.term_height - cursor_term_row
    else if cursor_term_row < 1 then 1 - cursor_term_row
    else 0
  in
  let prompt_top =
    let proposed =
      model.layout.prompt_top_row + scrolls_from_expansion
      + scrolls_from_cursor_movement
    in
    if content_height <= model.layout.term_height then
      min proposed (model.layout.term_height - content_height + 1) |> max 1
    else proposed
  in
  {
    model with
    layout =
      {
        model.layout with
        prompt_box_height = new_height;
        prompt_top_row = prompt_top;
        scroll_amount = scrolls_from_expansion;
      };
  }

let handle_resize new_width new_height model =
  if
    model.layout.term_width = new_width && model.layout.term_height = new_height
  then model
  else
    let model =
      {
        model with
        layout =
          { model.layout with term_width = new_width; term_height = new_height };
      }
    in
    let prompt_top = clamp_prompt_top new_height model.layout.prompt_top_row in
    {
      model with
      layout =
        {
          model.layout with
          prompt_top_row = prompt_top;
          prompt_box_height = min_prompt_height;
        };
    }

let grapheme_cluster_count text =
  match Unicode_string.of_string text with
  | Ok u -> Ok (Unicode_string.length u)
  | Error e -> Error e

let submit model =
  assert_model_invariants model;
  let result =
    match model.input.mode with
    | Readline _ -> Readline_mode.submit model
    | Shell -> Shell_mode.submit model
    | Ai -> Ai_mode.submit model
    | Normal -> Normal_mode.submit model
    | History_search _ ->
        ({ model with input = { model.input with mode = Normal } }, [])
  in
  assert_model_invariants (fst result);
  result

(* Key handler router *)
let apply_key key model =
  match model.input.mode with
  | Readline _ -> Readline_mode.apply_key key model
  | Shell -> Shell_mode.apply_key key model
  | Ai -> Ai_mode.apply_key key model
  | Normal -> Normal_mode.apply_key key model
  | History_search _ -> History_search.apply_key key model

let universal_corrections key model =
  model |> Completion_controller.filter_completions
  |> handle_vertical_cursor_movement
  |> fun s ->
  let flipping_through_history =
    match model.input.flipping_through_history with
    | Some 1 | None -> None
    | Some n -> Some (n - 1)
  in
  {
    s with
    input = { s.input with previous_key = Some key; flipping_through_history };
  }

let handle_key_input key model =
  let new_model, effects =
    model
    |> handle_resize model.layout.term_width model.layout.term_height
    |> apply_key key
  in
  (universal_corrections key new_model, effects)

let process_response model =
  match model.repl.backend_response with
  | None -> failwith "process_response called with no backend_response"
  | Some response ->
      History.record_response model.input.history response;
      let repl_output =
        match response with
        | Ffi_backend.Stdout s -> Output_text [ (`Raw, s) ]
        | Ffi_backend.Result s -> Output_text [ (`Raw, s) ]
        | Ffi_backend.R_error s -> Output_text [ (`Error, s) ]
        | Ffi_backend.Internal_error s ->
            Output_text [ (`Error, "Internal error: " ^ s) ]
        | Ffi_backend.Restarted s -> Output_text [ (`Error, s) ]
        | Ffi_backend.Image image -> Output_image image
        | Ffi_backend.Ai_output s -> Output_text [ (`Raw, s) ]
        | Ffi_backend.Ai_done -> Output_text []
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
        | Ffi_backend.Readline _ | Ffi_backend.Image _ | Ffi_backend.Ai_output _
          ->
            model.repl.awaiting_response
        (* Terminal responses *)
        | Ffi_backend.Done | Ffi_backend.Shutdown | Ffi_backend.Internal_error _
        | Ffi_backend.Restarted _ | Ffi_backend.Passthrough
        | Ffi_backend.Passthrough_end | Ffi_backend.Completions _
        | Ffi_backend.Ai_done ->
            false
      in
      let mode =
        match response with
        | Ffi_backend.Readline prompt ->
            let normalized_prompt = if prompt = "" then "input" else prompt in
            Frontend_types.Readline normalized_prompt
        | Ffi_backend.Done -> Frontend_types.Normal (* Reset on completion *)
        | _ -> model.input.mode (* Preserve current mode *)
      in
      {
        model with
        repl =
          {
            model.repl with
            backend_response = None;
            repl_output = Some repl_output;
            awaiting_response;
          };
        layout = { model.layout with scroll_amount = 0 };
        input = { model.input with mode };
      }

let update msg model =
  assert_model_invariants model;
  let model = { model with layout = { model.layout with scroll_amount = 0 } } in
  let result =
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
            match model.input.mode with
            | History_search _ ->
                ( { model with input = { model.input with completion = None } },
                  [] )
            | _ ->
                let in_completion_mode =
                  match model.input.completion with
                  | Some cs when Completion.is_in_completion_mode cs -> true
                  | _ -> false
                in
                if in_completion_mode then (model, [])
                else
                  let token_start =
                    model.input.cursor_pos - String.length token
                  in
                  let completion =
                    Completion.create ~token_start
                      (List.map Completion.backend_item items)
                  in
                  let m =
                    {
                      model with
                      input = { model.input with completion = Some completion };
                    }
                  in
                  (Completion_controller.filter_completions m, []))
        | Ffi_backend.Restarted _ ->
            let m =
              {
                model with
                repl = { model.repl with backend_response = Some response };
              }
              |> process_response |> handle_vertical_cursor_movement
            in
            ( m,
              [
                Repl_effect.BackgroundSubmit
                  (Printf.sprintf "options(width=%d)" model.layout.term_width);
              ] )
        | _ ->
            let m =
              {
                model with
                repl = { model.repl with backend_response = Some response };
              }
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
          | None ->
              { model with input = { model.input with completion = None } }
          | Some text ->
              let replaced = replace_range model token_start token_end text in
              {
                replaced with
                input = { replaced.input with completion = None };
              }
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
  in
  assert_model_invariants (fst result);
  result
