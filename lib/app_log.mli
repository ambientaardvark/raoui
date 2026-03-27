val init : unit -> string
val path : unit -> string option
val log_exception : ?context:string -> exn -> unit
val ensure_dir : string -> unit
