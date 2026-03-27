let read_theme_name path =
  if not (Sys.file_exists path) then None
  else
    let toml = Otoml.Parser.from_file path in
    match Otoml.find_opt toml Otoml.get_string [ "theme" ] with
    | Some _ as s -> s
    | None -> None
