open Common

type t = {
  name : string ;

  mutable pos : position ;

  mutable texture : Raylib.Texture.t ;
  (** current texture depicting the agent *)

  mutable color : Raylib.Color.t
  (** current color depicting agent *)
};;

let create (name : string) (pos : position) (color : Raylib.Color.t) (texture : Raylib.Texture.t) : t =
  {
    name ;
    pos = pos ;
    color = color;
    texture = texture ;
  }

let get_name (puppet : t) : string =
  puppet.name

let set_pos (puppet : t) (pos : position) =
  puppet.pos <- pos;;

let get_pos (puppet : t) : position =
  puppet.pos;;

let set_texture (puppet : t) (texture : Raylib.Texture.t) : unit =
  puppet.texture <- texture

let get_texture (puppet : t) : Raylib.Texture.t =
  puppet.texture;;

let get_color (puppet : t) : Raylib.Color.t =
  puppet.color

