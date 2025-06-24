open Common
open RegionAgentState
open BoardInterface

type t

type region_agent_class = {
  states : RegionAgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : RegionAgentState.t ;
  (** The state that the agent starts out in *)

  speed : float ;

  name : string ;
  (** The name of the agent class *)
}

val create : board_interface ->
             region_agent_class ->
             t
(** [create board agent_blueprint name] Creates an agent *)

val draw : t -> unit
(** [draw agent] draws [agent] to screen *)

val name : t -> string
(** The name of the region agent *)

val on_puppet_enter : t -> board_interface -> PuppetExternal.t -> unit
(** Called in response to a puppet entering this region *)

val on_puppet_exit : t -> board_interface -> PuppetExternal.t -> unit
(** Called in response to a puppet exiting this region *)

val resume : t -> unit
(** [resume agent] Resumes [region agent]'s script. *)

val handle_messages : t -> unit
(** Handle all messages from incoming channels *)
