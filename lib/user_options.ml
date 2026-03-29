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
}

let default = {
  theme_name = None;
  plot_mode = Auto;
  plot_renderer = Gr_devices;
}

let parse_plot_mode = function
  | "auto" -> Some Auto
  | "png" -> Some Png
  | "httpgd" -> Some Httpgd
  | "ide" -> Some Ide
  | "off" -> Some Off
  | _ -> None

let parse_plot_renderer = function
  | "gr_devices" -> Some Gr_devices
  | "ragg" -> Some Ragg
  | _ -> None

let read_string toml key =
  Otoml.find_opt toml Otoml.get_string [ key ]

let read path =
  if not (Sys.file_exists path) then default
  else
    try
      let toml = Otoml.Parser.from_file path in
      let theme_name = read_string toml "theme" in
      let plot_mode =
        match read_string toml "plot_mode" with
        | Some value -> (
            match parse_plot_mode value with
            | Some mode -> mode
            | None ->
                Logs.warn (fun m ->
                    m "unknown plot_mode %S in %s; using auto" value path);
                default.plot_mode)
        | None -> default.plot_mode
      in
      let plot_renderer =
        match read_string toml "plot_renderer" with
        | Some value -> (
            match parse_plot_renderer value with
            | Some renderer -> renderer
            | None ->
                Logs.warn (fun m ->
                    m "unknown plot_renderer %S in %s; using gr_devices"
                      value path);
                default.plot_renderer)
        | None -> default.plot_renderer
      in
      { theme_name; plot_mode; plot_renderer }
    with exn ->
      Logs.warn (fun m ->
          m "failed to parse %s: %s" path (Printexc.to_string exn));
      default

let read_theme_name path = (read path).theme_name

let string_of_plot_renderer = function
  | Gr_devices -> "gr_devices"
  | Ragg -> "ragg"
