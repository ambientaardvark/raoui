type color =
  | Default
  | Ansi256 of int

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
    accent = face (Ansi256 6);
    error = face (Ansi256 1);
    keyword = face (Ansi256 5);
    string = face (Ansi256 2);
    number = face (Ansi256 3);
    comment = face (Ansi256 8);
    operator = face (Ansi256 6);
    constant = face (Ansi256 4);
    ident = face Default;
    bracket = face Default;
    function_ = face (Ansi256 5);
    completion = face ~bg:(Ansi256 236) Default;
    completion_selected = face ~bg:(Ansi256 240) ~bold:true Default;
    shell_prompt = face (Ansi256 1);
  }

let tokyo_night =
  {
    name = "tokyo_night";
    plain = face (Ansi256 252);
    accent = face (Ansi256 117);
    error = face (Ansi256 210);
    keyword = face (Ansi256 141);
    string = face (Ansi256 150);
    number = face (Ansi256 221);
    comment = face (Ansi256 103);
    operator = face (Ansi256 117);
    constant = face (Ansi256 111);
    ident = face (Ansi256 252);
    bracket = face (Ansi256 183);
    function_ = face (Ansi256 81);
    completion = face ~bg:(Ansi256 238) (Ansi256 252);
    completion_selected = face ~bg:(Ansi256 60) ~bold:true (Ansi256 231);
    shell_prompt = face (Ansi256 204);
  }

let of_name = function
  | "default" -> default
  | "tokyo_night" -> tokyo_night
  | _ -> default
