type startup_mode =
  | Use_ide
  | Use_httpgd
  | Use_raoui_png
  | Use_none

val resolve :
  running_in_ide:bool ->
  terminal_caps:Terminal_capabilities.t ->
  plot_mode:User_options.plot_mode ->
  startup_mode

val startup_command :
  startup_mode ->
  renderer:User_options.plot_renderer ->
  string option
