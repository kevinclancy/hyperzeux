open BoardInterface
open Common

type t
(** The "mental state" of an ambient agent at a specific point in time.
    Should be related to some goal, e.g. display a text box *)

type _ Effect.t += RegionAction : unit -> unit Effect.t

type channel_handler = (t option, board_interface) Channel.t_in_handler

type 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (board_interface -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  create_handlers : ('s -> channel_handler list) option ;

  assert_invariants : (board_interface -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  on_puppet_enter : (board_interface -> PuppetExternal.t -> 's -> t option) option ;
  (** [on_puppet_enter board_intf puppet] is called when [puppet] enters this region *)

  on_puppet_exit : (board_interface -> PuppetExternal.t -> 's -> t option) option ;
  (** [on_puppet_exit board_intf puppet] is called when [puppet] exits this region *)
}

val empty_state_functions : 's state_functions

type blueprint_props = {
  (** Properties shared by all ambient agent state blueprints, regardless of private data type *)

  agent_set_invariant : StringSet.t ;
  (** A subset of the set of names of agents that should be contained in the region
      while this state is active *)

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

and resume_result =
  | Continue
  (** [Continue] Tells the agent to remain in its current state *)
  | ChangeState of t
  (** [ChangeState s] Tells the agent to change to state [s] *)

exception ChangeState of t

val create : 's blueprint -> 's -> t
(** [create blueprint initial_state] Creates a new agent state from [blueprint]
    using [initial_state] as initial state *)

val name : t -> string
(** [state_name s] is the name of the state [s] *)

val resume : t -> board_interface -> float -> resume_result
(** [resume state prev_result t_delta_seconds] Resume the [state]'s coroutine, where [prev_result] tells whether
    the previously yielded action suceeded *)

val handle_messages : t -> board_interface -> t option
(** [handle_messages state board_interface] handles incoming messages until one of the two conditions is encountered:

    * All message queues are empty. In this case, return None.

    * A message handler returns [Some(new_state)], triggering a state change to [new_state].
      In this case, [handle_messages] returns [Some(new_state)]

    where

    * [state] is the agent's current state

    * [board_interface] is the agent's view of the board
*)

val on_puppet_enter : t -> board_interface -> PuppetExternal.t -> t option
(** [on_puppet_enter state board puppet] called when [puppet] enters the region *)

val on_puppet_exit : t -> board_interface -> PuppetExternal.t -> t option
(** [on_puppet_enter state board puppet] called when [puppet] enters the region *)