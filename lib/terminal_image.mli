type rendered = {
  output : string;
  rows : int;
}

val render :
  terminal_capabilities:Terminal_capabilities.t ->
  config:User_options.t ->
  term_width:int ->
  image:Ffi_backend.image ->
  rendered option
