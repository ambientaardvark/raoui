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

let of_name = function
  | "default" -> default
  | "tokyo_night" -> tokyo_night
  | _ -> default
