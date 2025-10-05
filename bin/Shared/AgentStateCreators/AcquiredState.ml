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

type command_msg = script_fn * bool ref
(** A command message: a script function paired with a completion flag.
    The script function takes a puppet (the acquired agent), a board interface, and command functions for movement.
    The bool ref is set to true when the command completes. *)

type command_channel = command_msg Channel.t
(** A channel that transmits command messages *)

type acquire_msg = (command_channel option) ref
(** A message sent on an acquire channel.
    The sender passes a reference initialized to None.
    When the acquired agent enters the Acquired state, it creates a command channel
    and sets the reference to Some command_channel. *)

type acquired_state_kit = {
  acquire_handler : (AgentState.t option, board_interface * Puppet.t) Channel.t_in_handler ;
  (** A handler for the acquire channel that transitions it to the acquire state *)

  add_acquire_state : Agent.agent_class -> Agent.agent_class ;
  (** Add an "Acquired" state to the agent class *)

  acquire_channel : acquire_msg Channel.t
  (** An acquire channel that states may listen to explicitly instead of using [acquire_handler] *)

  (* TODO: add another function that constructs a new acquire state, needed for [acquire_channel] to be useful *)
}

(** Global map of acquire channels, indexed by agent name *)
let acquire_channels : (acquire_msg Channel.t) StringMap.t ref = ref StringMap.empty

let get_acquire_channel (agent_name : string) : acquire_msg Channel.t =
  StringMap.find agent_name !acquire_channels

let create (agent_name : string) (cmd_fns : command_functions) : acquired_state_kit =

  let acquire_channel : acquire_msg Channel.t = Channel.create ("acquire_" ^ agent_name) in
  acquire_channels := StringMap.add agent_name acquire_channel !acquire_channels;

  let rec acquired_state_blueprint (command_channel : command_channel) : unit AgentState.blueprint =
    {
      state_functions = {
        AgentState.empty_state_functions with
        create_handlers = Some(fun fields ->
          [
            Channel.attach_handler command_channel (fun command_msg (board_intf, puppet) ->
              Some (AgentState.create (runscript_state_blueprint command_channel command_msg) ())
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
    ((script, is_finished) : command_msg) : unit AgentState.blueprint = {

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
  let acquire_handler = Channel.attach_handler acquire_channel (fun acquire_msg _ ->
    let command_channel = Channel.create ("command_" ^ agent_name) in
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
  {
        acquire_handler;
        add_acquire_state;
        acquire_channel
  }
