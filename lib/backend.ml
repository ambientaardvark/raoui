type response_chunk =
  | Partial of string
  | Complete of string
  | Error of string

type completion = string

type t = {
  mutable pending_input: string option;
}

let create () = { pending_input = None }

let submit t input =
  t.pending_input <- Some input

let await_response t =
  match t.pending_input with
  | None -> Error "No pending request"
  | Some input ->
    t.pending_input <- None;
    Complete input

let cancel t =
  t.pending_input <- None

let get_completions _t _input ~cursor_pos:_ = []
