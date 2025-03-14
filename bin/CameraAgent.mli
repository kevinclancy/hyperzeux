open Common
open AmbientAgentState
open BoardInterface

type t

type camera_agent_class = {
  states : CameraAgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : CameraAgentState.t ;
  (** The state that the agent starts out in *)

  speed : float ;

  name : string ;
  (** The name of the agent class *)
}

val create : board_interface ->
             camera_agent_class ->
             t
(** [create board agent_blueprint] Creates an agent *)

val get_pos : t -> vec2

val name : t -> string
(** The name of the ambient agent *)

val update_input : t -> unit
(** Update agent state in response to user input *)

val resume : t -> float -> unit
(** [resume agent t_delta_seconds] Resumes [ambient agent]'s script. *)

val handle_messages : t -> unit
(** Handle all messages from incoming channels *)
