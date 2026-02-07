open Common
open AgentState
open BoardInterface
open Acquisition

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
