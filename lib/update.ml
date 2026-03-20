open Frontend_types

type msg =
  | Key of Tty_listener.key
  | Response of Ffi_backend.response_chunk
  | TermResize of int * int

let lexer_update start_line end_line model =
  match model.mode with
  | Normal | History_search _ ->
      {
        model with
        lex_cache =
          Syntax.Cache.update ~start_line ~end_line ~lines:model.lines
            model.lex_cache;
      }
  | _ -> { model with lex_cache = Syntax.Cache.make_all_default model.lines }

(* Cursor context primitives *)
let current_line model = List.nth model.lines model.cursor_line
let line_length model = Unicode_string.length (current_line model)

let char_at model =
  let line = current_line model in
  if model.cursor_pos >= Unicode_string.length line then None
  else Some (Unicode_string.cluster_at line model.cursor_pos)

let char_before model =
  if model.cursor_pos = 0 then None
  else
    Some (Unicode_string.cluster_at (current_line model) (model.cursor_pos - 1))

let at_line_start model = model.cursor_pos = 0
let at_line_end model = model.cursor_pos >= line_length model
let at_first_line model = model.cursor_line = 0
let at_last_line model = model.cursor_line >= List.length model.lines - 1

let same_cursor_pos m1 m2 =
  m1.cursor_line = m2.cursor_line && m1.cursor_pos = m2.cursor_pos

let prompt_is_empty model =
  model.lines = [] || model.lines = [ Unicode_string.empty ]

let insert_char model s =
  let width = effective_width model in
  let line = current_line model in
  match Unicode_string.insert_string line ~pos:model.cursor_pos s with
  | Error _ -> model (* Invalid UTF-8 byte, ignore it *)
  | Ok new_line ->
      let new_lines = update_line model.lines model.cursor_line new_line in
      let new_pos = model.cursor_pos + 1 in
      let new_row, new_col =
        internal_to_terminal width new_lines (model.cursor_line, new_pos)
      in
      {
        model with
        lines = new_lines;
        cursor_pos = new_pos;
        cursor_row = new_row;
        cursor_col = new_col;
      }
      |> lexer_update model.cursor_line model.cursor_line

let delete_char model =
  let width = effective_width model in
  if at_line_start model then
    if at_first_line model then model
    else
      let prev_line = List.nth model.lines (model.cursor_line - 1) in
      let curr_line = current_line model in
      let merged = Unicode_string.append prev_line curr_line in
      let new_lines =
        model.lines
        |> List.filteri (fun i _ -> i <> model.cursor_line)
        |> List.mapi (fun i line ->
            if i = model.cursor_line - 1 then merged else line)
      in
      let new_line_idx = model.cursor_line - 1 in
      let new_pos = Unicode_string.length prev_line in
      let new_row, new_col =
        internal_to_terminal width new_lines (new_line_idx, new_pos)
      in
      {
        model with
        lines = new_lines;
        cursor_line = new_line_idx;
        cursor_pos = new_pos;
        cursor_row = new_row;
        cursor_col = new_col;
      }
      |> lexer_update new_line_idx (new_line_idx + 1)
  else
    let line = current_line model in
    let new_line = Unicode_string.delete line (model.cursor_pos - 1) in
    let new_lines = update_line model.lines model.cursor_line new_line in
    let new_pos = model.cursor_pos - 1 in
    let new_row, new_col =
      internal_to_terminal width new_lines (model.cursor_line, new_pos)
    in
    {
      model with
      lines = new_lines;
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
    |> lexer_update model.cursor_line model.cursor_line

let insert_newline model =
  let width = effective_width model in
  let line = current_line model in
  let before, after = Unicode_string.split line model.cursor_pos in
  let new_lines =
    List.concat_map
      (fun (i, l) -> if i = model.cursor_line then [ before; after ] else [ l ])
      (List.mapi (fun i l -> (i, l)) model.lines)
  in
  let new_line_idx = model.cursor_line + 1 in
  let new_row, new_col =
    internal_to_terminal width new_lines (new_line_idx, 0)
  in
  {
    model with
    lines = new_lines;
    cursor_line = new_line_idx;
    cursor_pos = 0;
    cursor_row = new_row;
    cursor_col = new_col;
  }
  |> lexer_update model.cursor_line new_line_idx

let insert_paste model text =
  let max_len = 5 * 1024 in
  let text =
    if String.length text > max_len then String.sub text 0 max_len else text
  in
  let width = effective_width model in
  let line = current_line model in
  let before, after = Unicode_string.split line model.cursor_pos in
  let paste_lines = String.split_on_char '\n' text in
  let to_us s =
    match Unicode_string.of_string s with
    | Ok u -> u
    | Error _ -> Unicode_string.empty
  in
  let inserted, final_pos =
    match paste_lines with
    | [] -> ([ Unicode_string.append before after ], model.cursor_pos)
    | [ single ] ->
        let single_us = to_us single in
        ( [ Unicode_string.concat [ before; single_us; after ] ],
          model.cursor_pos + Unicode_string.length single_us )
    | first :: rest ->
        let rec split_last = function
          | [] -> ([], "")
          | [ x ] -> ([], x)
          | x :: xs ->
              let middle, last = split_last xs in
              (x :: middle, last)
        in
        let middle, last = split_last rest in
        let first_us = to_us first in
        let last_us = to_us last in
        let middle_us = List.map to_us middle in
        let first_line = Unicode_string.append before first_us in
        let last_line = Unicode_string.append last_us after in
        ( (first_line :: middle_us) @ [ last_line ],
          Unicode_string.length last_us )
  in
  let new_lines =
    List.concat_map
      (fun (i, l) -> if i = model.cursor_line then inserted else [ l ])
      (List.mapi (fun i l -> (i, l)) model.lines)
  in
  let final_line_idx = model.cursor_line + List.length paste_lines - 1 in
  let new_row, new_col =
    internal_to_terminal width new_lines (final_line_idx, final_pos)
  in
  {
    model with
    lines = new_lines;
    cursor_line = final_line_idx;
    cursor_pos = final_pos;
    cursor_row = new_row;
    cursor_col = new_col;
  }
  |> lexer_update model.cursor_line final_line_idx

let move_left model =
  let width = effective_width model in
  if not (at_line_start model) then
    let new_pos = model.cursor_pos - 1 in
    let new_row, new_col =
      internal_to_terminal width model.lines (model.cursor_line, new_pos)
    in
    {
      model with
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else if not (at_first_line model) then
    let new_line = model.cursor_line - 1 in
    let prev_line = List.nth model.lines new_line in
    let new_pos = Unicode_string.length prev_line in
    let new_row, new_col =
      internal_to_terminal width model.lines (new_line, new_pos)
    in
    {
      model with
      cursor_line = new_line;
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else model

let move_right model =
  let width = effective_width model in
  if not (at_line_end model) then
    let new_pos = model.cursor_pos + 1 in
    let new_row, new_col =
      internal_to_terminal width model.lines (model.cursor_line, new_pos)
    in
    {
      model with
      cursor_pos = new_pos;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else if not (at_last_line model) then
    let new_line = model.cursor_line + 1 in
    let new_row, new_col =
      internal_to_terminal width model.lines (new_line, 0)
    in
    {
      model with
      cursor_line = new_line;
      cursor_pos = 0;
      cursor_row = new_row;
      cursor_col = new_col;
    }
  else model

let move_up model =
  let width = effective_width model in
  let wrapped_lines = wrap_lines width model.lines in
  if model.cursor_row > 0 then
    let target_col =
      match model.previous_key with
      | Some Up | Some Down -> model.persistent_col
      | _ -> model.cursor_col
    in
    let line_width =
      List.nth wrapped_lines (model.cursor_row - 1)
      |> Unicode_string.display_width
    in
    let new_row = model.cursor_row - 1 in
    let new_col = min target_col line_width in
    let new_cursor_line, new_cursor_pos =
      terminal_to_internal width model.lines (new_row, new_col)
    in
    {
      model with
      cursor_row = new_row;
      cursor_col = new_col;
      cursor_line = new_cursor_line;
      cursor_pos = new_cursor_pos;
      persistent_col = target_col;
    }
  else model

let move_down model =
  let width = effective_width model in
  let wrapped_lines = wrap_lines width model.lines in
  let total_rows = List.length wrapped_lines in
  if model.cursor_row < total_rows - 1 then
    let target_col =
      match model.previous_key with
      | Some Up | Some Down -> model.persistent_col
      | _ -> model.cursor_col
    in
    let line_width =
      List.nth wrapped_lines (model.cursor_row + 1)
      |> Unicode_string.display_width
    in
    let new_row = model.cursor_row + 1 in
    let new_col = min target_col line_width in
    let new_cursor_line, new_cursor_pos =
      terminal_to_internal width model.lines (new_row, new_col)
    in
    {
      model with
      cursor_row = new_row;
      cursor_col = new_col;
      cursor_line = new_cursor_line;
      cursor_pos = new_cursor_pos;
      persistent_col = target_col;
    }
  else model

let user_input_char model c =
  match c with
  | "[" | "{" | "(" -> (
      let after1 = insert_char model c in
      match c with
      | _ when not (at_line_end model) -> after1
      | "[" -> move_left (insert_char after1 "]")
      | "(" -> move_left (insert_char after1 ")")
      | "{" -> move_left (insert_char after1 "}")
      | _ -> after1)
  | "]" | "}" | ")" -> (
      if at_line_end model then insert_char model c
      else
        match char_at model with
        | Some s when s = c -> move_right model
        | _ -> insert_char model c)
  | "'" | "\"" -> (
      if at_line_end model then insert_char (insert_char model c) c |> move_left
      else
        match char_at model with
        | Some s when s = c -> move_right model
        | _ -> insert_char (insert_char model c) c |> move_left)
  | _ -> insert_char model c

let user_input_delete model =
  if at_line_start model || at_line_end model then delete_char model
  else
    match (char_before model, char_at model) with
    | Some before_s, Some at_s ->
        let b = String.get before_s 0 in
        let a = String.get at_s 0 in
        let is_matched_pair =
          match b with
          | '(' -> a = ')'
          | '[' -> a = ']'
          | '{' -> a = '}'
          | '"' -> a = '"'
          | '\'' -> a = '\''
          | _ -> false
        in
        if is_matched_pair then
          model |> move_right |> delete_char |> delete_char
        else delete_char model
    | _ -> delete_char model

let move_cursor_to_end model =
  let width = effective_width model in
  let last_line_idx = List.length model.lines - 1 in
  let last_line = List.nth model.lines last_line_idx in
  let new_pos = Unicode_string.length last_line in
  let new_row, new_col =
    internal_to_terminal width model.lines (last_line_idx, new_pos)
  in
  {
    model with
    cursor_line = last_line_idx;
    cursor_pos = new_pos;
    cursor_row = new_row;
    cursor_col = new_col;
  }

let go_to_line_start model = { model with cursor_pos = 0; cursor_col = 0 }

let go_to_line_end model =
  let width = effective_width model in
  let new_pos = line_length model in
  let new_row, new_col =
    internal_to_terminal width model.lines (model.cursor_line, new_pos)
  in
  {
    model with
    cursor_pos = new_pos;
    cursor_row = new_row;
    cursor_col = new_col;
  }

let is_word_char s =
  if String.length s > 1 then true
  else
    match String.get s 0 with
    | ' ' | '\t' | '/' | ',' | '=' | '-' | '+' | '[' | ']' | '{' | '}' | '('
    | ')' | '|' | '\\' | '?' | '<' | '>' | '`' | '~' | '!' | '@' | '#' | '$'
    | '%' | '^' | '&' | '*' | ';' | ':' | '\'' | '"' ->
        false
    | _ -> true

let go_to_next_word model =
  let rec loop seen_word model =
    if at_line_end model then model
    else
      match char_at model with
      | None -> model
      | Some cursor_char ->
          if is_word_char cursor_char then
            let new_mod = move_right model in
            if same_cursor_pos model new_mod then model else loop true new_mod
          else if seen_word then model
          else
            let new_mod = move_right model in
            if same_cursor_pos model new_mod then model else loop false new_mod
  in
  loop false model

let go_to_last_word model =
  let rec loop seen_word model =
    if at_line_start model then model
    else
      match char_before model with
      | None -> model
      | Some cursor_char ->
          if is_word_char cursor_char then
            let new_mod = move_left model in
            if same_cursor_pos model new_mod then model else loop true new_mod
          else if seen_word then model
          else
            let new_mod = move_left model in
            if same_cursor_pos model new_mod then model else loop false new_mod
  in
  loop false model

let shift_history model ~amount =
  let result =
    if amount > 0 then
      History.go_back model.history ~current_prompt:model.lines ()
    else History.go_forwards model.history ~current_prompt:model.lines ()
  in
  match result with
  | Some lines ->
      {
        model with
        lines;
        lex_cache = Syntax.Cache.create lines;
        flipping_through_history = Some 2;
      }
      |> move_cursor_to_end
  | None -> model

let delete_before_cursor model =
  let width = effective_width model in
  if at_line_start model then delete_char model
  else
    let line = current_line model in
    let new_line =
      Unicode_string.delete_range line ~start:0 ~len:model.cursor_pos
    in
    let new_lines = update_line model.lines model.cursor_line new_line in
    let new_row, new_col =
      internal_to_terminal width new_lines (model.cursor_line, 0)
    in
    {
      model with
      lines = new_lines;
      cursor_pos = 0;
      cursor_row = new_row;
      cursor_col = new_col;
    }
    |> lexer_update model.cursor_line model.cursor_line

let delete_char_after_cursor model =
  let after_move_right = move_right model in
  if same_cursor_pos model after_move_right then model
  else delete_char after_move_right

let continuation_indent_size = 2

let tokens_before_cursor model =
  Syntax.Cache.tokens_before_line model.lex_cache ~line:(model.cursor_line + 1)

let mode_at_cursor model =
  let entry = Syntax.Cache.get_entry model.lex_cache ~line:model.cursor_line in
  match entry with
  | None -> R_lexer.Normal
  | Some entry ->
      let line = current_line model in
      let cursor_byte =
        Language_syntax.cursor_byte_offset ~line ~cursor_pos:model.cursor_pos
      in
      let prefix = String.sub entry.text 0 cursor_byte in
      let _, end_mode = R_lexer.lex_line entry.start_mode prefix in
      end_mode

let inside_empty_brackets model =
  match
    Syntax.Cache.get_line_tokens model.lex_cache ~line:model.cursor_line
  with
  | None -> false
  | Some tokens ->
      let line = current_line model in
      let cursor_byte =
        Language_syntax.cursor_byte_offset ~line ~cursor_pos:model.cursor_pos
      in
      Syntax.Continuation.inside_empty_brackets ~tokens
        ~cursor_byte_offset:cursor_byte

let leading_spaces s =
  let rec loop i =
    if i >= String.length s then i
    else if String.get s i = ' ' then loop (i + 1)
    else i
  in
  loop 0

let insert_spaces count model =
  let rec loop m n = if n <= 0 then m else loop (insert_char m " ") (n - 1) in
  loop model count

let expand_empty_brackets model =
  let line = current_line model in
  let base_indent = leading_spaces (Unicode_string.to_string line) in
  model |> insert_newline
  |> insert_spaces (base_indent + continuation_indent_size)
  |> insert_newline |> insert_spaces base_indent |> move_up |> go_to_line_end

let at_empty_line model =
  "" = (model |> current_line |> Unicode_string.to_string |> String.trim)

(* Submit handlers per mode *)

let scroll_terminal_after_submit model =
  let width = effective_width model in
  let wrapped = wrap_lines width model.lines in
  let total_rows = List.length wrapped in
  let output_row = model.prompt_top_row + total_rows in
  let new_prompt_top = output_row + 1 in
  let scroll_amount =
    if new_prompt_top > model.term_height then
      model.term_height - new_prompt_top
    else 0
  in
  (output_row, scroll_amount, new_prompt_top)

let clear_model_for_submit ?(awaiting_response = true) model =
  let output_row, scroll_amount, new_prompt_top =
    scroll_terminal_after_submit model
  in
  {
    model with
    awaiting_response;
    repl_cursor = (output_row + scroll_amount, 1);
    prompt_top_row = new_prompt_top + scroll_amount;
    previous_prompt_top_row = new_prompt_top + scroll_amount;
    lines = [ Unicode_string.empty ];
    lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
    prompt_box_height = min_prompt_height;
    scroll_amount;
  }

let submit_in_shell_mode model =
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
  let r_command = Printf.sprintf "system(r\"%s(%s)%s\")" guard text guard in
  History.add_to_history model.history model.lines;
  ( clear_model_for_submit { model with mode = Normal },
    [ Repl_effect.Submit r_command ] )

let submit_in_readline_mode model =
  let text = Unicode_string.to_string (current_line model) in
  let new_model =
    {
      model with
      mode = Frontend_types.Normal;
      lines = [ Unicode_string.empty ];
      lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
      cursor_row = 0;
      cursor_col = 0;
      cursor_line = 0;
      cursor_pos = 0;
    }
  in
  (new_model, [ Repl_effect.SubmitReadlineInput text ])

let submit_normal_text model =
  let text =
    String.concat "\n" (List.map Unicode_string.to_string model.lines)
  in
  History.add_to_history model.history model.lines;
  if String.equal (String.trim text) "q()" then (model, [ Repl_effect.Quit ])
  else (clear_model_for_submit model, [ Repl_effect.Submit text ])

let submit_in_normal_mode model =
  if inside_empty_brackets model then (expand_empty_brackets model, [])
  else if at_empty_line model then submit_normal_text model
  else
    let tokens = tokens_before_cursor model in
    match Syntax.Continuation.analyze tokens with
    | Syntax.Continuation.Submit -> submit_normal_text model
    | Syntax.Continuation.Continue { indent_levels; in_empty_brackets = _ } ->
        let line = current_line model in
        let base_indent = leading_spaces (Unicode_string.to_string line) in
        let indent_spaces =
          max base_indent (indent_levels * continuation_indent_size)
        in
        let rec repeat_n_times n f m =
          if n <= 0 then m else repeat_n_times (n - 1) f (f m)
        in
        ( model |> insert_newline
          |> repeat_n_times indent_spaces (fun m -> insert_char m " "),
          [] )

(* Submit router *)
let submit model =
  match model.mode with
  | Readline _ -> submit_in_readline_mode model
  | Shell -> submit_in_shell_mode model
  | Normal -> submit_in_normal_mode model
  | History_search _ -> ({ model with mode = Normal }, [])

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

let is_empty_input model =
  match model.lines with [ line ] -> Unicode_string.is_empty line | _ -> false

let replace_token model token_start text =
  let line = current_line model in
  let before = Unicode_string.sub line ~start:0 ~len:token_start in
  let after_start = model.cursor_pos in
  let after_len = Unicode_string.length line - after_start in
  let after = Unicode_string.sub line ~start:after_start ~len:after_len in
  let text_us =
    match Unicode_string.of_string text with
    | Ok u -> u
    | Error _ -> Unicode_string.empty
  in
  let new_line = Unicode_string.concat [ before; text_us; after ] in
  let new_lines = update_line model.lines model.cursor_line new_line in
  let new_cursor_pos = token_start + Unicode_string.length text_us in
  let model = { model with lines = new_lines; cursor_pos = new_cursor_pos } in
  let model = lexer_update model.cursor_line model.cursor_line model in
  let width = effective_width model in
  let new_row, new_col =
    internal_to_terminal width model.lines (model.cursor_line, new_cursor_pos)
  in
  { model with cursor_row = new_row; cursor_col = new_col }

let filter_completions model =
  match model.completion with
  | None -> model
  | Some cs when Completion.is_in_completion_mode cs ->
      model (* completion mode: set is fixed *)
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
        | Some filtered -> { model with completion = Some filtered })

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
        | Some completion_text ->
            let token_start = Completion.token_start cs_cycled in
            let inserted = replace_token model token_start completion_text in
            { inserted with completion = Some cs_cycled })

(* Key handlers per mode *)

let set_mode_normal_blank model =
  {
    model with
    mode = Frontend_types.Normal;
    lines = [ Unicode_string.empty ];
    lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
    cursor_row = 0;
    cursor_col = 0;
    cursor_line = 0;
    cursor_pos = 0;
  }

let apply_key_in_shell_mode key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' -> (set_mode_normal_blank model, [])
  | Ctrl 'p' | Up | Down -> (model, [])
  | Enter -> submit model
  | _ -> (
      let model =
        match key with
        | Tab | Escape -> model
        | _ -> (
            match model.completion with
            | Some cs when Completion.is_in_completion_mode cs ->
                { model with completion = None }
            | _ -> model)
      in
      match key with
      | Ctrl 'd' ->
          if is_empty_input model then (set_mode_normal_blank model, [])
          else (delete_char_after_cursor model, [])
      | Ctrl 'u' -> (delete_before_cursor model, [])
      | Ctrl 'a' -> (go_to_line_start model, [])
      | Ctrl 'e' -> (go_to_line_end model, [])
      | Other "next word" -> (go_to_next_word model, [])
      | Other "last word" -> (go_to_last_word model, [])
      | Char c -> (user_input_char model c, [])
      | Backspace when prompt_is_empty model -> (set_mode_normal_blank model, [])
      | Backspace -> (user_input_delete model, [])
      | Left -> (move_left model, [])
      | Right -> (move_right model, [])
      | Paste text -> (insert_paste model text, [])
      | _ -> (model, []))

let apply_key_in_readline_mode key model =
  let open Tty_listener in
  match key with
  | Ctrl 'c' ->
      (set_mode_normal_blank model, [ Repl_effect.SubmitReadlineInput "" ])
  | Ctrl 'p' | Up | Down -> (model, [])
  | Enter -> submit model
  | _ -> (
      let model =
        match key with
        | Tab | Escape -> model
        | _ -> (
            match model.completion with
            | Some cs when Completion.is_in_completion_mode cs ->
                { model with completion = None }
            | _ -> model)
      in
      match key with
      | Ctrl 'd' ->
          if is_empty_input model then (model, [ Repl_effect.Quit ])
          else (delete_char_after_cursor model, [])
      | Ctrl 'u' -> (delete_before_cursor model, [])
      | Ctrl 'a' -> (go_to_line_start model, [])
      | Ctrl 'e' -> (go_to_line_end model, [])
      | Other "next word" -> (go_to_next_word model, [])
      | Other "last word" -> (go_to_last_word model, [])
      | Char c -> (user_input_char model c, [])
      | Backspace -> (user_input_delete model, [])
      | Left -> (move_left model, [])
      | Right -> (move_right model, [])
      | Paste text -> (insert_paste model text, [])
      | _ -> (model, []))

let apply_key_in_normal_mode key model =
  let open Tty_listener in
  let model =
    match key with
    | Tab | Escape -> model
    | _ -> (
        match model.completion with
        | Some cs when Completion.is_in_completion_mode cs ->
            { model with completion = None }
        | _ -> model)
  in
  match key with
  | Ctrl 'c' when model.awaiting_response -> (model, [ Repl_effect.Cancel ])
  | Ctrl 'p' when model.awaiting_response -> (model, [])
  | Ctrl 'd' ->
      if is_empty_input model then (model, [ Repl_effect.Quit ])
      else (delete_char_after_cursor model, [])
  | Enter -> submit model
  | Ctrl 'u' -> (delete_before_cursor model, [])
  | Ctrl '\r' -> (insert_newline model, [])
  | Ctrl 'p' -> (shift_history model ~amount:1, [])
  | Ctrl 'r' ->
      ( {
          model with
          mode = History_search Unicode_string.empty;
          lines = [ Unicode_string.empty ];
          lex_cache = Syntax.Cache.create [ Unicode_string.empty ];
          cursor_pos = 0;
          cursor_col = 0;
          cursor_row = 0;
          cursor_line = 0;
        },
        [] )
  | Ctrl 'a' -> (go_to_line_start model, [])
  | Ctrl 'e' -> (go_to_line_end model, [])
  | Other "next word" -> (go_to_next_word model, [])
  | Other "last word" -> (go_to_last_word model, [])
  | Char ";" when prompt_is_empty model ->
      ({ model with mode = Frontend_types.Shell }, [])
  | Char c -> (user_input_char model c, [])
  | Backspace -> (user_input_delete model, [])
  | Left -> (move_left model, [])
  | Right -> (move_right model, [])
  | Up ->
      if at_first_line model || Option.is_some model.flipping_through_history
      then (shift_history model ~amount:1, [])
      else (move_up model, [])
  | Down ->
      if at_last_line model || Option.is_some model.flipping_through_history
      then (shift_history model ~amount:(-1), [])
      else (move_down model, [])
  | Paste text -> (insert_paste model text, [])
  | Tab -> (handle_tab model, [])
  | Escape -> (
      match model.completion with
      | Some cs when Completion.is_in_completion_mode cs ->
          let token_start = Completion.token_start cs in
          let original = Completion.original_token cs in
          let reverted = replace_token model token_start original in
          ({ reverted with completion = None }, [])
      | Some _ -> ({ model with completion = None }, [])
      | None -> (model, []))
  | _ -> (model, [])

(* Key handler router *)
let apply_key key model =
  match model.mode with
  | Readline _ -> apply_key_in_readline_mode key model
  | Shell -> apply_key_in_shell_mode key model
  | Normal -> apply_key_in_normal_mode key model
  | History_search _ -> History_search.apply_key key model

let universal_corrections key model =
  model |> filter_completions |> handle_vertical_cursor_movement |> fun s ->
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
  let model = model |> handle_resize model.term_width model.term_height in
  let model =
    match model.mode with
    | History_search _ -> model
    | _ -> sync_internal_coords model
  in
  let m, effects = apply_key key model in
  (universal_corrections key m, effects)

let process_response model =
  match model.backend_response with
  | None -> failwith "process_response called with no backend_response"
  | Some response ->
      let repl_output =
        match response with
        | Ffi_backend.Stdout s -> [ (`Raw, s) ]
        | Ffi_backend.Result s -> [ (`Raw, s) ]
        | Ffi_backend.R_error s -> [ (`Error, s) ]
        | Ffi_backend.Internal_error s -> [ (`Error, "Internal error: " ^ s) ]
        | Ffi_backend.Restarted s -> [ (`Error, s) ]
        | Ffi_backend.Done -> []
        | Ffi_backend.Shutdown -> []
        | Ffi_backend.Passthrough | Ffi_backend.Passthrough_end
        | Ffi_backend.Completions _ | Ffi_backend.Readline _ ->
            []
      in
      let awaiting_response =
        match response with
        (* Keep waiting for more output until we get a terminal response *)
        | Ffi_backend.Stdout _ | Ffi_backend.Result _ | Ffi_backend.R_error _
        | Ffi_backend.Readline _ ->
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

let completion_effects model =
  let in_completion_mode =
    match model.completion with
    | Some cs when Completion.is_in_completion_mode cs -> true
    | _ -> false
  in
  if (not model.awaiting_response) && not in_completion_mode then
    match List.nth_opt model.lines model.cursor_line with
    | Some line ->
        let text = Unicode_string.to_string line in
        [ Repl_effect.RequestCompletions (text, model.cursor_pos) ]
    | None -> []
  else []

let update msg model =
  let model = { model with scroll_amount = 0 } in
  match msg with
  | Key key -> (
      let m, effects = handle_key_input key model in
      match effects with [] -> (m, completion_effects m) | _ -> (m, effects))
  | Response response -> (
      match response with
      | Ffi_backend.Shutdown -> (model, [ Repl_effect.Quit ])
      | Ffi_backend.Passthrough -> (model, [ Repl_effect.EnterPassthrough ])
      | Ffi_backend.Passthrough_end -> (model, [])
      | Ffi_backend.Completions (token, items) ->
          let in_completion_mode =
            match model.completion with
            | Some cs when Completion.is_in_completion_mode cs -> true
            | _ -> false
          in
          if in_completion_mode then (model, [])
          else
            let token_start = model.cursor_pos - String.length token in
            let completion = Completion.create ~token_start items in
            let m = { model with completion = Some completion } in
            (filter_completions m, [])
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
  | TermResize (width, height) ->
      let m =
        handle_resize width height model |> handle_vertical_cursor_movement
      in
      ( m,
        [
          Repl_effect.BackgroundSubmit
            (Printf.sprintf "options(width=%d)" width);
        ] )
