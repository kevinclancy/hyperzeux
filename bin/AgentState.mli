
open BoardInterface

type t
(** The "mental state" of an agent at a specific point in time.
    Should be related to some goal, e.g. a state for walking to a waypoint *)

type resume_result =
  | PerformAction of Actions.action
  (** [PerformAction a] Tells the agent to submit action [a] to the board *)
  | ChangeState of t
  (** [ChangeState s] Tells the agent to change to state [s] and submit an [Idle] action to the board  *)


type 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (board_interface -> Puppet.t -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  create_handlers : ('s -> ((t option, board_interface * Puppet.t) Channel.t_in_handler) list) option ;

  assert_invariants : (board_interface -> Puppet.t -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  receive_bump : (board_interface -> Puppet.t -> 's -> PuppetExternal.t -> t option) option ;
  (** [receive_bump self private_data other] is called when [other] bumps into this agent *)

  key_left_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_left_pressed self private_data] called when the left-arrow key is pressed *)

  key_right_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_right_pressed self private_data] called when the right-arrow key is pressed *)

  key_up_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_up_pressed self private_data] callback called when the up-arrow key is pressed *)

  key_down_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_down_pressed self private_data] callback called when the down-arrow key is pressed *)

  key_left_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_left_released self private_data] called when the left-arrow key is pressed *)

  key_right_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_right_released self private_data] called when the right-arrow key is pressed *)

  key_up_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_up_released self private_data] callback called when the up-arrow key is pressed *)

  key_down_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_down_released self private_data] callback called when the down-arrow key is pressed *)
}

val empty_state_functions : 's state_functions

type blueprint_props = {
  (** Properties shared by all agent state blueprints, regardless of private data type *)

  region_name : string option ;
  (** Name of region that the agent is expected to remain within while in this state,
      or None if no such expectation exists *)

  name : string
  (** The name of this state *)
}

type 's blueprint = {
  (** Agent state blueprint, where ['s] is the type of the agent state's private data *)

  state_functions : 's state_functions ;
  (** Functions for interacting with agents in this state *)

  props : blueprint_props
  (** Properties shared by all agent state blueprints, regardless of private data type *)
}

exception ChangeState of t
(** ChangeState(s) signals a change to state [s] *)

val create : 's blueprint -> 's -> t
(** [create blueprint initial_state] Creates a new agent state from [blueprint]
    using [initial_state] as initial state *)

val name : t -> string
(** [state_name s] is the name of the state [s] *)

val region_name : t -> string option
(** [egion_name s] is the region we expect an agent to stay inside while in state [s] *)

val resume : t -> board_interface -> Puppet.t -> Actions.action_result -> resume_result
(** [resume state prev_result] Resume the [state]'s coroutine, where [prev_result] tells whether
    the previously yielded action suceeded *)

val handle_messages : t -> board_interface -> Puppet.t -> t option
(** [handle_messages state board_interface self] handles incoming messages until one of the two conditions is encountered:

    * All message queues are empty. In this case, return None.

    * A message handler returns [Some(new_state)], triggering a state change to [new_state].
      In this case, [handle_messages] returns [Some(new_state)]

    where

    * [state] is the current agent's current state

    * [board_interface] is the current agent's view of the board

    * [self] is the current agent's puppet
*)

val receive_bump : t -> board_interface -> Puppet.t -> PuppetExternal.t -> t option
(** [receive_bump state other] called when the [other] puppet bumps into the puppet that [state] is controlling *)

val key_left_pressed : t -> board_interface ->Puppet.t -> t option
(** [key_left state puppet] called when the left-arrow key is pressed *)

val key_right_pressed : t -> board_interface -> Puppet.t -> t option
(** [key_right state puppet] called when the right-arrow key is pressed *)

val key_up_pressed : t -> board_interface -> Puppet.t -> t option
(** [key_up state puppet] called when the up-arrow key is pressed *)

val key_down_pressed : t -> board_interface -> Puppet.t -> t option
(** [key_down state puppet] called when the down-arrow key is pressed *)

val key_left_released : t -> board_interface -> Puppet.t -> t option
(** [key_left_released state puppet] called when the left-arrow key is released *)

val key_right_released : t -> board_interface -> Puppet.t -> t option
(** [key_right_released state puppet] called when the right-arrow key is released *)

val key_up_released : t -> board_interface -> Puppet.t -> t option
(** [key_up_released state puppet] called when the up-arrow key is released *)

val key_down_released : t -> board_interface -> Puppet.t -> t option
(** [key_down_released state puppet] called when the down-arrow key is released *)