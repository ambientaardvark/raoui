type style =
  [ `Raw
  | `Plain
  | `Accent
  | `Error
  | `Keyword
  | `String
  | `Number
  | `Comment
  | `Operator
  | `Constant
  | `Ident
  | `Bracket
  | `Function
  | `Completion
  | `Completion_selected
  | `Shell_prompt
  ]
type span = style * string
type term_output = span list
type direction = Left of int | Right of int | Up of int | Down of int

type op =
  | Print of term_output
  | Print_char of char
  | Newline
  | Cursor_to of int * int
  | Cursor_shift of direction
  | Clear_to_eol
  | Show_cursor
  | Hide_cursor

module type TERMINAL = sig
  val render : Theme.t -> op Queue.t -> string
  val render_spans : Theme.t -> span list -> string
  val cursor_to : int -> int -> string
  val clear_to_eos : string
  val solid_cursor : string
  val enable_bracketed_paste : string
  val disable_bracketed_paste : string
  val cursor_position_request : string
  val scroll_up : term_height:int -> int -> string
end


let sgr_codes_of_color ~is_background color =
  match Theme.to_ansi256 color with
  | None -> [ if is_background then "49" else "39" ]
  | Some n -> [ (if is_background then "48" else "38"); "5"; string_of_int n ]

let face_to_ansi face =
  let codes = ref (sgr_codes_of_color ~is_background:false face.Theme.fg) in
  (match face.Theme.bg with
  | Some bg -> codes := !codes @ sgr_codes_of_color ~is_background:true bg
  | None -> codes := !codes @ [ "49" ]);
  if face.Theme.bold then codes := "1" :: !codes;
  "\x1b[" ^ String.concat ";" !codes ^ "m"

let style_to_face theme = function
  | `Plain -> theme.Theme.plain
  | `Accent -> theme.Theme.accent
  | `Error -> theme.Theme.error
  | `Keyword -> theme.Theme.keyword
  | `String -> theme.Theme.string
  | `Number -> theme.Theme.number
  | `Comment -> theme.Theme.comment
  | `Operator -> theme.Theme.operator
  | `Constant -> theme.Theme.constant
  | `Ident -> theme.Theme.ident
  | `Bracket -> theme.Theme.bracket
  | `Function -> theme.Theme.function_
  | `Completion -> theme.Theme.completion
  | `Completion_selected -> theme.Theme.completion_selected
  | `Shell_prompt -> theme.Theme.shell_prompt
  | `Raw -> theme.Theme.plain

let style_to_ansi theme = function
  | `Raw -> "\x1b[0m"
  | style -> face_to_ansi (style_to_face theme style)

let render_spans_to_buf theme buf spans =
  List.iter (fun (style, text) ->
    Buffer.add_string buf (style_to_ansi theme style);
    Buffer.add_string buf text
  ) spans;
  Buffer.add_string buf "\x1b[0m"  (* reset after *)

let render_spans theme spans =
  let buf = Buffer.create 256 in
  render_spans_to_buf theme buf spans;
  Buffer.contents buf

let render_op_ansi theme buf op =
  let csi = "\x1b[" in
  match op with
  | Print spans -> render_spans_to_buf theme buf spans
  | Print_char c -> Buffer.add_char buf c
  | Newline -> Buffer.add_char buf '\n'
  | Cursor_to (row, col) -> Printf.bprintf buf "%s%d;%dH" csi row col
  | Cursor_shift dir -> (
      match dir with
      | Up n -> Printf.bprintf buf "%s%dA" csi n
      | Down n -> Printf.bprintf buf "%s%dB" csi n
      | Right n -> Printf.bprintf buf "%s%dC" csi n
      | Left n -> Printf.bprintf buf "%s%dD" csi n)
  | Clear_to_eol -> Printf.bprintf buf "%sK" csi
  | Show_cursor -> Printf.bprintf buf "%s?25h" csi
  | Hide_cursor -> Printf.bprintf buf "%s?25l" csi

module Ansi : TERMINAL = struct
  let render theme ops =
    let buf = Buffer.create 256 in
    Queue.iter (render_op_ansi theme buf) ops;
    Buffer.contents buf

  let render_spans = render_spans
  let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col
  let clear_to_eos = "\x1b[J"
  let solid_cursor = "\x1b[2 q"
  let enable_bracketed_paste = "\x1b[?2004h"
  let disable_bracketed_paste = "\x1b[?2004l"
  let cursor_position_request = "\x1b[6n"

  let scroll_up ~term_height n =
    let buf = Buffer.create (16 + n) in
    Printf.bprintf buf "\x1b[%d;1H" term_height;
    for _ = 1 to n do Buffer.add_char buf '\n' done;
    Buffer.contents buf
end
