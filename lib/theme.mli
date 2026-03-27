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

val default : t
val tokyo_night : t
val of_name : string -> t
