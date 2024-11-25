open Common

type board_interface = {
  get_waypoint : string -> position
  (** Retrieve the waypoint with the given name *)
}

module type AgentClass = sig
  val create : board_interface -> string -> Common.position -> Raylib.Color.t -> Agent.t
  (** [create board_intf name pos color] Creates an agent named [name] at position [pos] with color [color] *)

  val preview_texture_name : string
  (** The name of the texture used to represent the agent class in the map editor *)

  val preview_color : Raylib.Color.t
  (** The color that the preview texture is drawn in the map editor *)

  val name : string
  (** The name of the agent class *)
end
