open Common

(** [create name pos] Creates a patroller named [name] at position [pos] *)
val create : string -> position -> Agent.t

(** The name of this agent class: "Patroller" *)
val name : string

(** The name of the texture used to represent patrollers in the map editor *)
val preview_texture_name : string

(** The color used to display patroller in the map editor *)
val preview_color : Raylib.Color.t