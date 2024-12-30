  (** The state of an agent at a point in time. Includes a coroutine ("script"),
      callbacks for responding to events, and private data.
    *)

type t
(** The "mental state" of an agent at a specific point in time.
    Should be related to some goal, e.g. a state for walking to a waypoint *)

type 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (Puppet.t -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  assert_invariants : (Puppet.t -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  receive_bump : (Puppet.t -> 's -> PuppetExternal.t -> t option) option ;
  (** [receive_bump self private_data other] is called when [other] bumps into this agent *)

  key_left_pressed : (Puppet.t -> 's -> t option) option ;
  (** [key_left_pressed self private_data] called when the left-arrow key is pressed *)

  key_right_pressed : (Puppet.t -> 's -> t option) option ;
  (** [key_right_pressed self private_data] called when the right-arrow key is pressed *)

  key_up_pressed : (Puppet.t -> 's -> t option) option ;
  (** [key_up_pressed self private_data] callback called when the up-arrow key is pressed *)

  key_down_pressed : (Puppet.t -> 's -> t option) option ;
  (** [key_down_pressed self private_data] callback called when the down-arrow key is pressed *)

  key_left_released : (Puppet.t -> 's -> t option) option ;
  (** [key_left_released self private_data] called when the left-arrow key is pressed *)

  key_right_released : (Puppet.t -> 's -> t option) option ;
  (** [key_right_released self private_data] called when the right-arrow key is pressed *)

  key_up_released : (Puppet.t -> 's -> t option) option ;
  (** [key_up_released self private_data] callback called when the up-arrow key is pressed *)

  key_down_released : (Puppet.t -> 's -> t option) option ;
  (** [key_down_released self private_data] callback called when the down-arrow key is pressed *)
}

val empty_state_functions : 's state_functions

module type AgentStateClass = sig
  (** Definition of a state of an agent. *)

  type t_private_data
  (** Type of private data of an instance of this agent state *)

  val state_functions : unit -> t_private_data state_functions
  (** Functions for interacting with agents in this state, where 't is the agent type *)

  val region_name : unit -> string option
  (** Name of region that the agent is expected to remain within while in this state,
      or None if no such expectation exists *)

  val name : unit -> string
  (** The name of this state *)
end

val create : (module AgentStateClass with type t_private_data = 's) -> 's -> t

val name : t -> string
(** [state_name s] is the name of the state [s] *)

val region_name : t -> string option
(** [egion_name s] is the region we expect an agent to stay inside while in state [s] *)

val resume : t -> Puppet.t -> Actions.action_result -> Actions.action
(** [resume state prev_result] Resume the [state]'s coroutine, where [prev_result] tells whether
    the previously yielded action suceeded *)

val receive_bump : t -> Puppet.t -> PuppetExternal.t -> t option
(** [receive_bump state other] called when the [other] puppet bumps into the puppet that [state] is controlling *)

val key_left_pressed : t -> Puppet.t -> t option
(** [key_left state puppet] called when the left-arrow key is pressed *)

val key_right_pressed : t -> Puppet.t -> t option
(** [key_right state puppet] called when the right-arrow key is pressed *)

val key_up_pressed : t -> Puppet.t -> t option
(** [key_up state puppet] called when the up-arrow key is pressed *)

val key_down_pressed : t -> Puppet.t -> t option
(** [key_down state puppet] called when the down-arrow key is pressed *)

val key_left_released : t -> Puppet.t -> t option
(** [key_left_released state puppet] called when the left-arrow key is released *)

val key_right_released : t -> Puppet.t -> t option
(** [key_right_released state puppet] called when the right-arrow key is released *)

val key_up_released : t -> Puppet.t -> t option
(** [key_up_released state puppet] called when the up-arrow key is released *)

val key_down_released : t -> Puppet.t -> t option
(** [key_down_released state puppet] called when the down-arrow key is released *)