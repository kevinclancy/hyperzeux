open Common
open AgentState
open BoardInterface

type t

type agent_class = {
  states : AgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : AgentState.t ;
  (** The state that the agent starts out in *)

  preview_texture_name : string ;
  (** The name of the texture used to represent the agent class in the map editor *)

  preview_color : Raylib.Color.t ;
  (** The color that the preview texture is drawn in the map editor *)

  speed : float ;

  name : string ;
  (** The name of the agent class *)
}

val create : board_interface ->
             agent_class ->
             string ->
             Common.position ->
             Raylib.Color.t ->
             t
(** [create board agent_template name speed pos color] Creates an agent *)

val name : t -> string

val position : t -> position

val color : t -> Raylib.Color.t

val puppet : t -> Puppet.t

val set_position : t -> position -> unit

val set_state : t -> AgentState.t -> unit

val texture : t -> Raylib.Texture.t

val update_input : t -> unit
(** Update agent state in response to user input *)

val receive_bump : t -> PuppetExternal.t -> unit
(** [receive_bump self other] called when [other] bumps into [self] *)

val resume : t -> Actions.action_result -> Actions.action
(** [resume agent prev_action_result] Resumes [agent]'s script, providing the result of its previous action
    [prev_action_result] to the script's current context. *)

val handle_messages : t -> unit
(** Handle all messages from incoming channels *)
