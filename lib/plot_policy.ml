type startup_mode =
  | Use_ide
  | Use_httpgd
  | Use_raoui_png
  | Use_none

let resolve ~running_in_ide ~terminal_caps ~plot_mode =
  match plot_mode with
  | User_options.Ide -> Use_ide
  | User_options.Httpgd -> Use_httpgd
  | User_options.Png -> Use_raoui_png
  | User_options.Off -> Use_none
  | User_options.Auto ->
      if running_in_ide then Use_ide
      else if Terminal_capabilities.supports_inline_images terminal_caps then
        Use_raoui_png
      else
        Use_httpgd

let string_of_startup_mode = function
  | Use_ide -> "ide"
  | Use_httpgd -> "httpgd"
  | Use_raoui_png -> "png"
  | Use_none -> "off"

let startup_command startup_mode ~renderer =
  let renderer = User_options.string_of_plot_renderer renderer in
  match startup_mode with
  | Use_ide ->
      Some (Printf.sprintf "raoui_use_ide_viewer(renderer = %S)" renderer)
  | Use_httpgd ->
      Some (Printf.sprintf "raoui_use_httpgd(renderer_fallback = %S)" renderer)
  | Use_raoui_png ->
      Some (Printf.sprintf "raoui_use_png_device(renderer = %S)" renderer)
  | Use_none -> None
