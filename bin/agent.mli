type t

val get_pos : t -> int * int

val set_pos : t -> int * int -> unit

val get_texture : t -> Raylib.Texture.t

val set_texture : t -> Raylib.Texture.t -> unit

(** [create script pos] Creates a new agent executing [script] starting at [pos] *)
val create : (t -> unit) -> (int * int) -> Raylib.Texture.t -> t

val resume : t -> Actions.action

