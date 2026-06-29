type standard_color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white

type color =
  | Default
  | Standard of standard_color
  | Rgb of int * int * int
  | Greyscale of int

val to_ansi256 : color -> int option

type face = {
  fg : color;                (* foreground color *)
  bg : color option;         (* background color, None = terminal default *)
  bold : bool;               (* SGR 1 *)
  dim : bool;                (* SGR 2 (faint) *)
  italic : bool;             (* SGR 3 *)
  underline : bool;          (* SGR 4 *)
  strike : bool;             (* SGR 9 (crossed out) *)
}

type t = {
  name : string;
  plain : face;
  accent : face;
  error : face;
  keyword : face;
  string : face;
  number : face;
  comment : face;
  operator : face;
  constant : face;
  ident : face;
  bracket : face;
  function_ : face;
  completion : face;
  completion_selected : face;
  shell_prompt : face;
}

val default : t
val tokyo_night : t
val gruvbox : t
val catppuccin : t
val solarized : t
val dracula : t
val nord : t
val of_name : string -> t
