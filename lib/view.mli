module Make (_ : Terminal_ops.TERMINAL) : sig
  val view_ops : Frontend_types.model -> Terminal_ops.op Queue.t
  val view : Frontend_types.model -> string
end
