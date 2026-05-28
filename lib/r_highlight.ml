open Terminal_ops
module Lexer = R_lexer

type styled_range = { start_byte : int; end_byte : int; style : style }

let style_of_token : Lexer.token -> style = function
  | NUMBER _ -> `Number
  | STRING _ -> `String
  | COMMENT _ -> `Comment
  | KEYWORD _ -> `Keyword
  | CONSTANT _ -> `Constant
  | OPERATOR _ -> `Operator
  | IDENT _ -> `Ident
  | BACKTICK_IDENT _ -> `Ident
  | LAMBDA -> `Function
  | PUNCTUATION _ -> `Plain
  | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACKET | RIGHT_BRACKET | LEFT_BRACE
  | RIGHT_BRACE ->
      `Bracket
  | WHITESPACE _ -> `Plain
  | UNKNOWN _ -> `Error
  | DEFAULT _ -> `Plain
  | EOF -> `Plain

let token_to_lexeme = Lexer.token_to_lexeme

let token_to_span (token : Lexer.token) : span =
  (style_of_token token, token_to_lexeme token)

let parse_interpolation (s : string) (start : int) : span list * int =
  let len = String.length s in
  let rec find_close depth p =
    if p >= len then None
    else
      match s.[p] with
      | '{' -> find_close (depth + 1) (p + 1)
      | '}' when depth = 1 -> Some p
      | '}' -> find_close (depth - 1) (p + 1)
      | _ -> find_close depth (p + 1)
  in
  match find_close 1 start with
  | None -> ([ (`String, "{" ^ String.sub s start (len - start)) ], len)
  | Some close_pos ->
      let expr = String.sub s start (close_pos - start) in
      let expr_tokens, _ = Lexer.lex_line Lexer.Normal expr in
      let expr_spans = List.map token_to_span expr_tokens in
      ([ (`Bracket, "{") ] @ expr_spans @ [ (`Bracket, "}") ], close_pos + 1)

let parse_glue_string (s : string) : span list =
  let len = String.length s in
  if len < 2 || s.[0] <> '"' then [ (`String, s) ]
  else
    let has_close = s.[len - 1] = '"' in
    let inner =
      if has_close then String.sub s 1 (len - 2) else String.sub s 1 (len - 1)
    in
    let inner_len = String.length inner in
    let rec parse acc pos =
      if pos >= inner_len then List.rev acc
      else
        match inner.[pos] with
        | '{' when pos + 1 < inner_len && inner.[pos + 1] = '{' ->
            parse ((`String, "{{") :: acc) (pos + 2)
        | '{' ->
            let spans, next_pos = parse_interpolation inner (pos + 1) in
            parse (List.rev_append spans acc) next_pos
        | '}' when pos + 1 < inner_len && inner.[pos + 1] = '}' ->
            parse ((`String, "}}") :: acc) (pos + 2)
        | '}' -> parse ((`String, "}") :: acc) (pos + 1)
        | _ ->
            let rec find_brace p =
              if p >= inner_len then p
              else
                match inner.[p] with '{' | '}' -> p | _ -> find_brace (p + 1)
            in
            let end_pos = find_brace pos in
            parse
              ((`String, String.sub inner pos (end_pos - pos)) :: acc)
              end_pos
    in
    let inner_spans = parse [] 0 in
    ((`String, "\"") :: inner_spans)
    @ if has_close then [ (`String, "\"") ] else []

let tokens_to_spans (tokens : Lexer.token list) : span list =
  let rec loop acc = function
    | [] -> List.rev acc
    | Lexer.IDENT f :: Lexer.LEFT_PAREN :: Lexer.STRING s :: tl
      when f = "glue" || f = "str_glue" ->
        let glue_spans = parse_glue_string s in
        loop
          (List.rev_append glue_spans
             ((`Bracket, "(") :: (`Function, f) :: acc))
          tl
    | Lexer.IDENT func_name :: Lexer.LEFT_PAREN :: tl ->
        loop ((`Bracket, "(") :: (`Function, func_name) :: acc) tl
    | hd :: tl -> loop (token_to_span hd :: acc) tl
  in
  loop [] tokens

let merge_ranges ranges =
  let rec loop acc = function
    | [] -> List.rev acc
    | range :: rest -> (
        match acc with
        | prev :: acc_rest
          when prev.style = range.style && prev.end_byte = range.start_byte ->
            loop ({ prev with end_byte = range.end_byte } :: acc_rest) rest
        | _ -> loop (range :: acc) rest)
  in
  loop [] ranges

let normalize_ranges ~text_len ranges =
  let sorted =
    List.sort
      (fun a b ->
        match compare a.start_byte b.start_byte with
        | 0 -> compare a.end_byte b.end_byte
        | n -> n)
      ranges
  in
  let rec loop pos acc = function
    | [] ->
        let acc =
          if pos < text_len then
            { start_byte = pos; end_byte = text_len; style = `Plain } :: acc
          else acc
        in
        merge_ranges (List.rev acc)
    | range :: rest ->
        let start_byte = max pos (max 0 range.start_byte) in
        let end_byte = min text_len range.end_byte in
        let acc =
          if pos < start_byte then
            { start_byte = pos; end_byte = start_byte; style = `Plain } :: acc
          else acc
        in
        if end_byte <= start_byte then loop pos acc rest
        else
          loop end_byte
            ({ start_byte; end_byte; style = range.style } :: acc)
            rest
  in
  loop 0 [] sorted

let shift_range offset (range : styled_range) =
  {
    range with
    start_byte = range.start_byte + offset;
    end_byte = range.end_byte + offset;
  }

let rec ranges_for_tokens tokens =
  let positioned = Lexer_cache.tokens_with_positions tokens ~token_to_lexeme in
  let rec loop acc = function
    | [] -> List.rev acc
    | (Lexer.IDENT f, func_start, func_end)
      :: (Lexer.LEFT_PAREN, paren_start, paren_end)
      :: (Lexer.STRING s, string_start, _)
      :: rest
      when f = "glue" || f = "str_glue" ->
        let glue_ranges = glue_string_ranges ~start_byte:string_start s in
        let ranges =
          [
            { start_byte = func_start; end_byte = func_end; style = `Function };
            { start_byte = paren_start; end_byte = paren_end; style = `Bracket };
          ]
          @ glue_ranges
        in
        loop (List.rev_append ranges acc) rest
    | (Lexer.IDENT _, start_byte, end_byte)
      :: ((Lexer.LEFT_PAREN, _, _) :: _ as rest) ->
        loop ({ start_byte; end_byte; style = `Function } :: acc) rest
    | (token, start_byte, end_byte) :: rest ->
        let style = style_of_token token in
        loop ({ start_byte; end_byte; style } :: acc) rest
  in
  loop [] positioned

and glue_string_ranges ~start_byte s =
  let len = String.length s in
  if len < 2 || s.[0] <> '"' then
    [ { start_byte; end_byte = start_byte + len; style = `String } ]
  else
    let has_close = s.[len - 1] = '"' in
    let inner_end = if has_close then len - 1 else len in
    let rec find_close depth pos =
      if pos >= inner_end then None
      else
        match s.[pos] with
        | '{' -> find_close (depth + 1) (pos + 1)
        | '}' when depth = 1 -> Some pos
        | '}' -> find_close (depth - 1) (pos + 1)
        | _ -> find_close depth (pos + 1)
    in
    let rec parse acc pos =
      if pos >= inner_end then acc
      else
        match s.[pos] with
        | '{' when pos + 1 < inner_end && s.[pos + 1] = '{' ->
            parse
              ({
                 start_byte = start_byte + pos;
                 end_byte = start_byte + pos + 2;
                 style = `String;
               }
              :: acc)
              (pos + 2)
        | '{' -> (
            match find_close 1 (pos + 1) with
            | None ->
                {
                  start_byte = start_byte + pos;
                  end_byte = start_byte + inner_end;
                  style = `String;
                }
                :: acc
            | Some close_pos ->
                let expr_start = pos + 1 in
                let expr_len = close_pos - expr_start in
                let expr = String.sub s expr_start expr_len in
                let expr_tokens, _ = Lexer.lex_line Lexer.Normal expr in
                let expr_ranges =
                  ranges_for_tokens expr_tokens
                  |> normalize_ranges ~text_len:expr_len
                  |> List.map (shift_range (start_byte + expr_start))
                in
                let ranges =
                  [
                    {
                      start_byte = start_byte + pos;
                      end_byte = start_byte + pos + 1;
                      style = `Bracket;
                    };
                  ]
                  @ expr_ranges
                  @ [
                      {
                        start_byte = start_byte + close_pos;
                        end_byte = start_byte + close_pos + 1;
                        style = `Bracket;
                      };
                    ]
                in
                parse (List.rev_append ranges acc) (close_pos + 1))
        | '}' when pos + 1 < inner_end && s.[pos + 1] = '}' ->
            parse
              ({
                 start_byte = start_byte + pos;
                 end_byte = start_byte + pos + 2;
                 style = `String;
               }
              :: acc)
              (pos + 2)
        | '}' ->
            parse
              ({
                 start_byte = start_byte + pos;
                 end_byte = start_byte + pos + 1;
                 style = `String;
               }
              :: acc)
              (pos + 1)
        | _ ->
            let rec find_brace p =
              if p >= inner_end then p
              else match s.[p] with '{' | '}' -> p | _ -> find_brace (p + 1)
            in
            let end_pos = find_brace pos in
            parse
              ({
                 start_byte = start_byte + pos;
                 end_byte = start_byte + end_pos;
                 style = `String;
               }
              :: acc)
              end_pos
    in
    let ranges =
      [ { start_byte; end_byte = start_byte + 1; style = `String } ]
      |> fun acc ->
      parse acc 1 |> fun acc ->
      if has_close then
        {
          start_byte = start_byte + len - 1;
          end_byte = start_byte + len;
          style = `String;
        }
        :: acc
      else acc |> List.rev
    in
    merge_ranges ranges

let ranges_for_entry (entry : R_lex_cache.entry) =
  ranges_for_tokens entry.tokens
  |> normalize_ranges ~text_len:(String.length entry.text)

let highlight_line (mode : Lexer.mode) (line : string) : span list * Lexer.mode
    =
  let tokens, mode_out = Lexer.lex_line mode line in
  (tokens_to_spans tokens, mode_out)
