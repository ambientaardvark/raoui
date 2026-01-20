type response_chunk =
  | Partial of string
  | Complete of string
  | Error of string

type completion = string

type t = 
  { sleep: float -> unit
  ; mutable pending_input: string option
  }

let create clock = { pending_input = None; sleep = Eio.Time.sleep clock; }

let submit t input =
  t.pending_input <- Some input

let await_response t =
  match t.pending_input with
  | None -> Error "No pending request"
  | Some input ->
    t.sleep 2.0;
    t.pending_input <- None;
    Complete input

let cancel t =
  t.pending_input <- None

let get_completions _t _input ~cursor_pos:_ = []
