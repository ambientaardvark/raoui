open Base

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
  | Scroll_up of int
  | Scroll_down of int
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
  | `String -> "\x1b[32m"   (* green *)
  | `Number -> "\x1b[33m"   (* yellow *)
  | `Comment -> "\x1b[90m"  (* bright black/gray *)
  | `Operator -> "\x1b[36m" (* cyan *)
  | `Constant -> "\x1b[34m" (* blue *)
  | `Ident -> "\x1b[0m"     (* default *)
  | `Bracket -> "\x1b[0m"   (* default *)

let render_spans_to_buf buf spans =
  List.iter spans ~f:(fun (style, text) ->
    Buffer.add_string buf (style_to_ansi style);
    Buffer.add_string buf text
  );
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
  | Scroll_up n -> Printf.bprintf buf "%s%dS" csi n
  | Scroll_down n -> Printf.bprintf buf "%s%dT" csi n
  | Show_cursor -> Printf.bprintf buf "%s?25h" csi
  | Hide_cursor -> Printf.bprintf buf "%s?25l" csi

module Make (C : CONFIG) : TERMINAL = struct
  let render ops =
    let buf = Buffer.create 256 in
    match C.term_type with
    | "ansi" ->
        Queue.iter ops ~f:(render_op_ansi buf);
        Buffer.contents buf
    | _ -> failwith ("Unsupported terminal type: " ^ C.term_type)
end
