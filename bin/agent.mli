open Common

type t

val get_pos : t -> position

val set_pos : t -> position -> unit

val get_texture : t -> Raylib.Texture.t

val set_texture : t -> Raylib.Texture.t -> unit

(* do I need to include a texture as an argument below, or could new agents just use some default tex? *)
(** [create name script pos texture] Creates a new agent executing [script] starting at [pos] *)
val create : string -> (t -> unit) -> position -> Raylib.Texture.t -> t

val resume : t -> Actions.action

val compare : t -> t -> int
