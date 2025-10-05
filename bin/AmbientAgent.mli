open Common
open AmbientAgentState
open BoardInterface

type t

type ambient_agent_class = {
  states : AmbientAgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : AmbientAgentState.t ;
  (** The state that the agent starts out in *)

  name : string ;
  (** The name of the agent class *)
}

val create : board_interface ->
             ambient_agent_class ->
             t
(** [create board agent_blueprint name] Creates an agent *)

val draw : t -> unit
(** [draw agent] draws [agent] to screen *)

val name : t -> string
(** The name of the ambient agent *)

val update_input : t -> unit
(** Update agent state in response to user input *)

val resume : t -> unit
(** [resume agent] Resumes [ambient agent]'s script. *)

val handle_messages : t -> unit
(** Handle all messages from incoming channels *)
