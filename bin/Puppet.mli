open Common

type t
(** The interface that an agent's control scripts have to itself *)

val create : string  -> position -> Raylib.Color.t -> Raylib.Texture.t -> t
(** [create name pos color texture] Creates a new puppet *)

val get_pos : t -> position
(** Get the current position of the agent *)

val set_pos : t -> position -> unit
(** Set the current position of the agent *)

val get_texture : t -> Raylib.Texture.t
(** Get the current texture of the agent *)

val set_texture : t -> Raylib.Texture.t -> unit
(** Set the current texture of the agent *)

val get_name : t -> string
(** Get the name of the agent *)

val get_color : t -> Raylib.Color.t
(** Get the current color of the agent *)