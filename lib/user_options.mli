type plot_mode =
  | Auto
  | Png
  | Httpgd
  | Ide
  | Off

type plot_renderer =
  | Gr_devices
  | Ragg

type t = {
  theme_name : string option;
  plot_mode : plot_mode;
  plot_renderer : plot_renderer;
  inline_image_max_width_cols : int;
  inline_image_max_height_rows : int;
}

val default : t
val read : string -> t
val read_theme_name : string -> string option
val string_of_plot_renderer : plot_renderer -> string
