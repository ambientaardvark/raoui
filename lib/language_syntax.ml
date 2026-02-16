(* language_syntax.ml *)

(** Language-agnostic lexer interface *)
module type LEXER = sig
  type token
  type mode

  val initial_mode : mode
  val lex_line : mode -> string -> token list * mode
end

(** Functor for building a cached incremental lexer *)
module Make (L : LEXER) = struct
  type entry = {
    text : string;
    tokens : L.token list;
    start_mode : L.mode;
    end_mode : L.mode;
  }

  type t = entry list

  (** Create a fresh cache for the given lines *)
  let create lines =
    let rec loop mode acc = function
      | [] -> List.rev acc
      | line :: rest ->
          let text = Unicode_string.to_string line in
          let tokens, end_mode = L.lex_line mode text in
          let entry = { text; tokens; start_mode = mode; end_mode } in
          loop end_mode (entry :: acc) rest
    in
    loop L.initial_mode [] lines

  (** Incrementally update cache when lines change *)
  let update ~start_line ~end_line ~lines cache =
    (* If cache structure is inconsistent, rebuild from scratch *)
    if List.length lines <> List.length cache then
      create lines
    else
      let rec loop i mode lines_rest cache_rest acc =
        match lines_rest with
        | [] -> List.rev acc
        | line :: lines_tail ->
            if i < start_line then
              (* Before changed region: reuse cached entry *)
              match cache_rest with
              | cached :: cache_tail ->
                  loop (i + 1) cached.end_mode lines_tail cache_tail (cached :: acc)
              | [] -> List.rev acc
            else if i > end_line && mode = L.initial_mode then
              (* After changed region and back to normal mode: try to reuse rest *)
              match cache_rest with
              | cached :: cache_tail when cached.start_mode = L.initial_mode ->
                  (* Can reuse the rest of the cache *)
                  List.rev acc @ (cached :: cache_tail)
              | _ ->
                  (* Can't reuse, continue lexing *)
                  let text = Unicode_string.to_string line in
                  let tokens, end_mode = L.lex_line mode text in
                  let entry = { text; tokens; start_mode = mode; end_mode } in
                  let cache_tail = match cache_rest with _ :: tl -> tl | [] -> [] in
                  loop (i + 1) end_mode lines_tail cache_tail (entry :: acc)
            else
              (* In changed region or mode still propagating: re-lex *)
              let text = Unicode_string.to_string line in
              let tokens, end_mode = L.lex_line mode text in
              let entry = { text; tokens; start_mode = mode; end_mode } in
              let cache_tail = match cache_rest with _ :: tl -> tl | [] -> [] in
              loop (i + 1) end_mode lines_tail cache_tail (entry :: acc)
      in
      loop 0 L.initial_mode lines cache []

  (** Get all tokens from the beginning up to (not including) [line] *)
  let tokens_before_line cache ~line =
    cache
    |> List.filteri (fun i _ -> i < line)
    |> List.map (fun entry -> entry.tokens)
    |> List.concat

  (** Get all tokens up to and including [line] *)
  let tokens_through_line cache ~line =
    cache
    |> List.filteri (fun i _ -> i <= line)
    |> List.map (fun entry -> entry.tokens)
    |> List.concat

  (** Get the entry for a specific line *)
  let get_entry cache ~line =
    List.nth_opt cache line

  (** Get tokens for a specific line *)
  let get_line_tokens cache ~line =
    get_entry cache ~line |> Option.map (fun entry -> entry.tokens)

  (** Get the lexer mode at the start of a given line *)
  let get_start_mode cache ~line =
    get_entry cache ~line |> Option.map (fun entry -> entry.start_mode)
end

(** Signature for language-specific continuation detection *)
module type CONTINUATION = sig
  type token

  type signal =
    | Submit
    | Continue of {
        indent_levels : int;
        in_empty_brackets : bool;
      }

  val analyze : token list -> signal

  val inside_empty_brackets :
    tokens:token list ->
    cursor_byte_offset:int ->
    bool
end

(** Calculate the byte offset of a cursor position within a line *)
let cursor_byte_offset ~line ~cursor_pos =
  if cursor_pos >= Unicode_string.length line then
    Unicode_string.byte_length line
  else
    Unicode_string.sub line ~start:0 ~len:cursor_pos
    |> Unicode_string.byte_length

(** Find tokens with their byte positions *)
let tokens_with_positions tokens ~token_to_lexeme =
  let rec loop pos acc = function
    | [] -> List.rev acc
    | tok :: rest ->
        let lexeme = token_to_lexeme tok in
        let len = String.length lexeme in
        let next_pos = pos + len in
        loop next_pos ((tok, pos, next_pos) :: acc) rest
  in
  loop 0 [] tokens
