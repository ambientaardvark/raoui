type image_protocol =
  | Kitty
  | ITerm
  | No_image

type t = {
  image_protocol : image_protocol;
}

val detect : ?getenv:(string -> string option) -> unit -> t
val supports_inline_images : t -> bool
