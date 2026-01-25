open Base

type term_output = string
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

let render_op_ansi buf op =
  let csi = "\x1b[" in
  match op with
  | Print s -> Buffer.add_string buf s
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
