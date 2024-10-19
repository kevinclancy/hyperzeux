module type AgentClass = sig
  (** [create name pos] Creates an agent named [name] at position [pos] *)
  val create : string -> Common.position -> Agent.t

  (** The name of the texture used to represent the agent class in the map editor *)
  val preview_texture_name : string

  (** The color that the preview texture is drawn in the map editor *)
  val preview_color : Raylib.Color.t

  (** The name of the agent class *)
  val name : string
end
