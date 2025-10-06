open BoardInterface
open Common

type t
(** The "mental state" of an ambient agent at a specific point in time.
    Should be related to some goal, e.g. display a text box *)

type _ Effect.t += CameraAction : unit -> unit Effect.t

type 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (board_interface -> float ref -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  create_handlers : ('s -> ((t option, board_interface) Channel.t_in_handler) list) option ;

  get_viewports : 's -> camera_transform list ;
  (** Gets a list of camera transforms to use for each layer that we're currently displaying
      Layers are listed from back to front, e.g. a GUI layer should be listed last *)

  assert_invariants : (board_interface -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  key_left_pressed : (board_interface -> 's -> t option) option ;
  (** [key_left_pressed self private_data] called when the left-arrow key is pressed *)

  key_right_pressed : (board_interface -> 's -> t option) option ;
  (** [key_right_pressed self private_data] called when the right-arrow key is pressed *)

  key_up_pressed : (board_interface -> 's -> t option) option ;
  (** [key_up_pressed self private_data] callback called when the up-arrow key is pressed *)

  key_down_pressed : (board_interface -> 's -> t option) option ;
  (** [key_down_pressed self private_data] callback called when the down-arrow key is pressed *)

  key_left_released : (board_interface -> 's -> t option) option ;
  (** [key_left_released self private_data] called when the left-arrow key is pressed *)

  key_right_released : (board_interface -> 's -> t option) option ;
  (** [key_right_released self private_data] called when the right-arrow key is pressed *)

  key_up_released : (board_interface -> 's -> t option) option ;
  (** [key_up_released self private_data] callback called when the up-arrow key is pressed *)

  key_down_released : (board_interface -> 's -> t option) option ;
  (** [key_down_released self private_data] callback called when the down-arrow key is pressed *)

  key_space_pressed : (board_interface -> 's -> t option) option ;

  key_space_released : (board_interface -> 's -> t option) option
}

and resume_result =
  | MaintainState
  (** [MaintainState] Tells the camera agent to continue in its current state *)
  | ChangeState of t
  (** [ChangeState s] Tells the camera agent to change to state [s] and submit an [Idle] action to the board  *)

val empty_state_functions : 's state_functions

type blueprint_props = {
  (** Properties shared by all ambient agent state blueprints, regardless of private data type *)

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

val get_viewports : t -> camera_transform list
(** Gets a list of camera transforms to use for each layer that we're currently displaying
    Layers are listed from back to front, e.g. a GUI layer should be listed last *)

val name : t -> string
(** [state_name s] is the name of the state [s] *)

val resume : t -> board_interface -> float -> resume_result
(** [resume state board_interface t_delta_seconds] Resume the [state]'s coroutine.
    Returns Some(new_state) if a state change was requested, None otherwise. *)

val handle_messages : t -> board_interface -> t option
(** [handle_messages state board_interface] handles incoming messages until one of the two conditions is encountered:

    * All message queues are empty. In this case, return None.

    * A message handler returns [Some(new_state)], triggering a state change to [new_state].
      In this case, [handle_messages] returns [Some(new_state)]

    where

    * [state] is the current agent's current state

    * [board_interface] is the current agent's view of the board
*)

val key_left_pressed : t -> board_interface -> t option
(** [key_left state puppet] called when the left-arrow key is pressed *)

val key_right_pressed : t -> board_interface -> t option
(** [key_right state puppet] called when the right-arrow key is pressed *)

val key_up_pressed : t -> board_interface -> t option
(** [key_up state puppet] called when the up-arrow key is pressed *)

val key_down_pressed : t -> board_interface -> t option
(** [key_down state puppet] called when the down-arrow key is pressed *)

val key_left_released : t -> board_interface -> t option
(** [key_left_released state puppet] called when the left-arrow key is released *)

val key_right_released : t -> board_interface -> t option
(** [key_right_released state puppet] called when the right-arrow key is released *)

val key_up_released : t -> board_interface -> t option
(** [key_up_released state puppet] called when the up-arrow key is released *)

val key_down_released : t -> board_interface -> t option
(** [key_down_released state puppet] called when the down-arrow key is released *)

val key_space_pressed : t -> board_interface -> t option

val key_space_released : t -> board_interface -> t option