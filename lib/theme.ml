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

let to_ansi256 = function
  | Default -> None
  | Standard c ->
      Some
        (match c with
        | Black -> 0
        | Red -> 1
        | Green -> 2
        | Yellow -> 3
        | Blue -> 4
        | Magenta -> 5
        | Cyan -> 6
        | White -> 7
        | Bright_black -> 8
        | Bright_red -> 9
        | Bright_green -> 10
        | Bright_yellow -> 11
        | Bright_blue -> 12
        | Bright_magenta -> 13
        | Bright_cyan -> 14
        | Bright_white -> 15)
  | Rgb (r, g, b) -> Some (16 + (36 * r) + (6 * g) + b)
  | Greyscale n -> Some (232 + n)

type face = {
  fg : color;
  bg : color option;
  bold : bool;
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

let face ?bg ?(bold = false) fg = { fg; bg; bold }

let default =
  {
    name = "default";
    plain = face Default;
    accent = face (Standard Cyan);
    error = face (Standard Red);
    keyword = face (Standard Magenta);
    string = face (Standard Green);
    number = face (Standard Yellow);
    comment = face (Standard Bright_black);
    operator = face (Standard Cyan);
    constant = face (Standard Blue);
    ident = face Default;
    bracket = face Default;
    function_ = face (Standard Magenta);
    completion = face ~bg:(Greyscale 4) Default;
    completion_selected = face ~bg:(Greyscale 8) ~bold:true Default;
    shell_prompt = face (Standard Red);
  }

let tokyo_night =
  {
    name = "tokyo_night";
    plain = face (Greyscale 20);
    accent = face (Rgb (2, 4, 5));
    error = face (Rgb (5, 2, 2));
    keyword = face (Rgb (3, 2, 5));
    string = face (Rgb (3, 4, 2));
    number = face (Rgb (5, 4, 1));
    comment = face (Rgb (2, 2, 3));
    operator = face (Rgb (2, 4, 5));
    constant = face (Rgb (2, 3, 5));
    ident = face (Greyscale 20);
    bracket = face (Rgb (4, 3, 5));
    function_ = face (Rgb (1, 4, 5));
    completion = face ~bg:(Greyscale 6) (Greyscale 20);
    completion_selected = face ~bg:(Rgb (1, 1, 2)) ~bold:true (Rgb (5, 5, 5));
    shell_prompt = face (Rgb (5, 1, 2));
  }

let gruvbox =
  {
    name = "gruvbox";
    plain = face (Rgb (5, 4, 3));
    accent = face (Rgb (4, 3, 1));
    error = face (Rgb (5, 1, 1));
    keyword = face (Rgb (5, 1, 1));
    string = face (Rgb (4, 4, 1));
    number = face (Rgb (4, 3, 3));
    comment = face (Greyscale 12);
    operator = face (Rgb (5, 3, 0));
    constant = face (Rgb (4, 3, 3));
    ident = face (Rgb (5, 4, 3));
    bracket = face (Rgb (3, 3, 3));
    function_ = face (Rgb (5, 4, 1));
    completion = face ~bg:(Greyscale 5) (Rgb (5, 4, 3));
    completion_selected = face ~bg:(Rgb (4, 3, 1)) ~bold:true (Greyscale 3);
    shell_prompt = face (Rgb (4, 4, 1));
  }

let catppuccin =
  {
    name = "catppuccin";
    plain = face (Rgb (4, 4, 5));
    accent = face (Rgb (3, 4, 5));
    error = face (Rgb (5, 3, 3));
    keyword = face (Rgb (4, 3, 5));
    string = face (Rgb (3, 4, 3));
    number = face (Rgb (5, 4, 3));
    comment = face (Greyscale 11);
    operator = face (Rgb (2, 4, 4));
    constant = face (Rgb (5, 4, 3));
    ident = face (Rgb (4, 4, 5));
    bracket = face (Greyscale 15);
    function_ = face (Rgb (3, 4, 5));
    completion = face ~bg:(Greyscale 5) (Rgb (4, 4, 5));
    completion_selected = face ~bg:(Greyscale 9) ~bold:true (Rgb (4, 4, 5));
    shell_prompt = face (Rgb (3, 4, 3));
  }

let solarized =
  {
    name = "solarized";
    plain = face (Greyscale 13);
    accent = face (Rgb (1, 3, 4));
    error = face (Rgb (4, 1, 1));
    keyword = face (Rgb (3, 3, 0));
    string = face (Rgb (1, 3, 3));
    number = face (Rgb (4, 1, 3));
    comment = face (Rgb (2, 2, 2));
    operator = face (Rgb (3, 2, 0));
    constant = face (Rgb (4, 1, 0));
    ident = face (Greyscale 13);
    bracket = face (Greyscale 11);
    function_ = face (Rgb (1, 3, 4));
    completion = face ~bg:(Rgb (0, 1, 1)) (Greyscale 13);
    completion_selected = face ~bg:(Rgb (2, 2, 2)) ~bold:true (Greyscale 23);
    shell_prompt = face (Rgb (3, 3, 0));
  }

let dracula =
  {
    name = "dracula";
    plain = face (Greyscale 23);
    accent = face (Rgb (4, 3, 5));
    error = face (Rgb (5, 2, 2));
    keyword = face (Rgb (5, 2, 4));
    string = face (Rgb (5, 5, 3));
    number = face (Rgb (4, 3, 5));
    comment = face (Rgb (2, 2, 3));
    operator = face (Rgb (5, 2, 4));
    constant = face (Rgb (4, 3, 5));
    ident = face (Greyscale 23);
    bracket = face (Greyscale 23);
    function_ = face (Rgb (2, 5, 2));
    completion = face ~bg:(Greyscale 7) (Greyscale 23);
    completion_selected = face ~bg:(Rgb (2, 2, 3)) ~bold:true (Greyscale 23);
    shell_prompt = face (Rgb (2, 5, 2));
  }

let nord =
  {
    name = "nord";
    plain = face (Greyscale 21);
    accent = face (Rgb (3, 4, 4));
    error = face (Rgb (4, 2, 2));
    keyword = face (Rgb (3, 3, 4));
    string = face (Rgb (3, 4, 3));
    number = face (Rgb (3, 2, 3));
    comment = face (Rgb (2, 2, 3));
    operator = face (Rgb (3, 3, 4));
    constant = face (Rgb (4, 3, 2));
    ident = face (Greyscale 21);
    bracket = face (Greyscale 21);
    function_ = face (Rgb (3, 4, 4));
    completion = face ~bg:(Greyscale 6) (Greyscale 21);
    completion_selected = face ~bg:(Greyscale 8) ~bold:true (Greyscale 22);
    shell_prompt = face (Rgb (3, 4, 3));
  }

let of_name = function
  | "default" -> default
  | "tokyo_night" -> tokyo_night
  | "gruvbox" -> gruvbox
  | "catppuccin" -> catppuccin
  | "solarized" -> solarized
  | "dracula" -> dracula
  | "nord" -> nord
  | _ -> default
