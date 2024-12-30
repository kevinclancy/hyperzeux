open Common
open AgentState

type board_interface = {
  get_waypoint : string -> position
  (** Retrieve the waypoint with the given name *)
}

type t

module type AgentClass = sig
  val states : (module AgentStateClass) StringMap.t
  (** Maps name of each state that agents of this class can enter to the state itself *)

  val initial_state : AgentState.t
  (** The state that the agent starts out in *)

  val preview_texture_name : string
  (** The name of the texture used to represent the agent class in the map editor *)

  val preview_color : Raylib.Color.t
  (** The color that the preview texture is drawn in the map editor *)

  val speed : float

  val name : string
  (** The name of the agent class *)
end

val create : board_interface ->
             (module AgentClass) ->
             AgentState.t ->
             string ->
             Common.position ->
             Raylib.Color.t ->
             t
(** [create board_interface agent_class initial_state name speed pos color] Creates an agent *)

val name : t -> string

val position : t -> position

val color : t -> Raylib.Color.t

val set_position : t -> position -> unit

val texture : t -> Raylib.Texture.t

val agent_class : t -> (module AgentClass)

val update_input : t -> unit
(** Update agent state in response to user input *)

val receive_bump : t -> PuppetExternal.t -> unit
(** [receive_bump self other] called when [other] bumps into [self] *)

val resume : t -> Actions.action_result -> Actions.action
(** [resume agent prev_action_result] Resumes [agent]'s script, providing the result of its previous action
    [prev_action_result] to the script's current context. *)