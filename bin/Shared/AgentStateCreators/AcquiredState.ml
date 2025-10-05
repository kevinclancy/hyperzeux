open Common
open AgentState
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

let create (acquire_channel : acquire_msg Channel.t) (cmd_fns : command_functions) : acquired_state_kit =
  (** [create acquire_channel cmd_fns] creates an acquired state kit for an agent.
      [acquire_channel] is the channel that will be used to acquire this agent.
      [cmd_fns] are the command functions for moving the agent.
      The return state must be set later using set_return_state. *)

  let return_state_ref : AgentState.t option ref = ref None in

  let rec acquired_state_blueprint (command_channel : command_channel) : unit AgentState.blueprint =
    {
      state_functions = {
        AgentState.empty_state_functions with
        create_handlers = Some(fun fields ->
          [
            Channel.attach_handler command_channel (fun command_msg (board_intf, puppet) ->
              match command_msg with
              | Command (script, is_finished) ->
                Some (AgentState.create (runscript_state_blueprint command_channel (script, is_finished)) ())
              | Release is_finished ->
                is_finished := true;
                match !return_state_ref with
                | Some state -> Some state
                | None -> failwith "Agent was released before set_return_state was called"
            )
          ]
        );
      } ;

      props = {
        region_name = None ;
        name = "Acquired" ;
      }
    }
  and runscript_state_blueprint
    (command_channel : command_msg Channel.t)
    ((script, is_finished) : script_fn * bool ref) : unit AgentState.blueprint = {

    state_functions = {
      AgentState.empty_state_functions with

      script = Some (fun (board : board_interface) (puppet : Puppet.t) () ->
        let open AgentScriptFunctions in
        script puppet board cmd_fns;
        is_finished := true;
        set_state (AgentState.create (acquired_state_blueprint command_channel) ())
      ) ;

      create_handlers = Some (fun () ->
        [
          (Channel.attach_handler
            command_channel
            (fun _ _ -> failwith "Received command while already running a script"))
        ]
      ) ;
    } ;

    props = {
      region_name = None ;
      name = "Running Script" ;
    }
  } in
  let channel_name = Channel.get_name acquire_channel in
  let acquire_handler = Channel.attach_handler acquire_channel (fun acquire_msg _ ->
    let command_channel = Channel.create ("command_" ^ channel_name) in
    acquire_msg := Some command_channel;
    Some (AgentState.create (acquired_state_blueprint command_channel) ())
  ) in
  let add_acquire_state (agent_class : Agent.agent_class) : Agent.agent_class =
    let running_script_props = {
      region_name = None ;
      name = "Running Script"
    } in
    let acquired_props = {
      region_name = None ;
      name = "Acquired"
    } in
    {
      agent_class with
        states =
          agent_class.states
          |> StringMap.add "Running Script" running_script_props
          |> StringMap.add "Acquired" acquired_props
    }
  in
  let set_return_state (state : AgentState.t) : unit =
    return_state_ref := Some state
  in

  {
        acquire_handler;
        add_acquire_state;
        set_return_state
  }
