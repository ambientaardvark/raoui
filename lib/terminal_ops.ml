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
  val render : op Queue.t -> string
end

module type CONFIG = sig
  val term_type : string
end

let style_to_ansi = function
  | `Raw -> "\x1b[0m"       (* reset before pass-through content *)
  | `Plain -> "\x1b[0m"     (* reset to default *)
  | `Accent -> "\x1b[36m"   (* cyan *)
  | `Error -> "\x1b[31m"    (* red *)
  | `Keyword -> "\x1b[35m"  (* magenta *)
  | `Function -> "\x1b[35m"  (* magenta *)
  | `String -> "\x1b[32m"   (* green *)
  | `Number -> "\x1b[33m"   (* yellow *)
  | `Comment -> "\x1b[90m"  (* bright black/gray *)
  | `Operator -> "\x1b[36m" (* cyan *)
  | `Constant -> "\x1b[34m" (* blue *)
  | `Ident -> "\x1b[0m"     (* default *)
  | `Bracket -> "\x1b[0m"   (* default *)
  | `Completion -> "\x1b[48;5;236m"  (* dark gray background *)
  | `Completion_selected -> "\x1b[48;5;240m\x1b[1m"  (* lighter gray bg, bold *)
  | `Shell_prompt -> "\x1b[31m" (* red *)

let render_spans_to_buf buf spans =
  List.iter (fun (style, text) ->
    Buffer.add_string buf (style_to_ansi style);
    Buffer.add_string buf text
  ) spans;
  Buffer.add_string buf "\x1b[0m"  (* reset after *)

let render_spans spans =
  let buf = Buffer.create 256 in
  render_spans_to_buf buf spans;
  Buffer.contents buf

let render_op_ansi buf op =
  let csi = "\x1b[" in
  match op with
  | Print spans -> render_spans_to_buf buf spans
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

let cursor_to row col = Printf.sprintf "\x1b[%d;%dH" row col
let clear_to_eos = "\x1b[J"
let solid_cursor = "\x1b[2 q"
let enable_bracketed_paste = "\x1b[?2004h"
let disable_bracketed_paste = "\x1b[?2004l"
let cursor_position_request = "\x1b[6n"

(* Scroll the viewport up by n lines using natural scrolling (newlines at the
   bottom row) so that the displaced lines are preserved in the scrollback
   buffer.  CSI n S discards lines in many terminal emulators. *)
let scroll_up ~term_height n =
  let buf = Buffer.create (16 + n) in
  Printf.bprintf buf "\x1b[%d;1H" term_height;
  for _ = 1 to n do Buffer.add_char buf '\n' done;
  Buffer.contents buf

module Make (C : CONFIG) : TERMINAL = struct
  let render ops =
    let buf = Buffer.create 256 in
    match C.term_type with
    | "ansi" ->
        Queue.iter (render_op_ansi buf) ops;
        Buffer.contents buf
    | _ -> failwith ("Unsupported terminal type: " ^ C.term_type)
end
