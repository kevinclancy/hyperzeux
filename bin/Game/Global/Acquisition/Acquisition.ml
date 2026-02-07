open BoardInterface

type command_functions = {
  walk_north : Puppet.t -> unit ;
  walk_east : Puppet.t -> unit ;
  walk_south : Puppet.t -> unit ;
  walk_west : Puppet.t -> unit ;
  walk_to : Puppet.t -> board_interface -> string -> unit ;
  (** [walk_to puppet board_interface waypoint] walks [puppet] to the waypoint named [waypoint] *)
}
(** Functions that command the acquired agent to walk in the four cardinal directions *)

type script_fn = Puppet.t -> BoardInterface.board_interface -> command_functions -> unit
(** The type of a script that we command the agent owning this state to run *)

type command_msg =
  | Command of script_fn * bool ref
  (** A command: a script function paired with a completion flag.
      The script function takes a puppet (the acquired agent), a board interface, and command functions for movement.
      The bool ref is set to true when the command completes. *)
  | Release of bool ref
  (** A release message: tells the agent to return to its return state.
      The bool ref is set to true when the agent has transitioned. *)

type command_channel = command_msg Channel.t
(** A channel that transmits command messages *)

type acquire_msg = (command_channel option) ref
(** A message sent on an acquire channel.
    The sender passes a reference initialized to None.
    When the acquired agent enters the Acquired state, it creates a command channel
    and sets the reference to Some command_channel. *)

type acquired_state_kit = {
  acquire_handler : (AgentState.t option, board_interface * Puppet.t) Channel.t_in_handler ;
  (** A handler for the acquire channel that transitions the agent to the acquire state *)

  add_acquire_state : Agent.agent_class -> Agent.agent_class ;
  (** Add an "Acquired" state to the agent class *)

  set_return_state : AgentState.t -> unit
  (** Set the return state for this acquired agent. Must be called before the agent is acquired. *)
}