open Common
open AgentClass_intf
val create : board_interface -> string -> position -> Raylib.Color.t -> Agent.t
(** [create name pos color] Creates a player named [name] at position [pos] with color [color] *)

val name : string
(** The name of this agent class: "player" *)

val preview_texture_name : string
(** The name of the texture used to represent this agent in the map editor *)

val preview_color : Raylib.Color.t
(** The color "skyblue" used to display this agent in the map editor *)