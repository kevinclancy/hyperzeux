open Common
open RegionAgentState
open RegionAgent
open BoardInterface

type trigger_filter =
  | PlayerOnly
  | PuppetNamed of string
  | AnyPuppet

let create ?(filter = PlayerOnly) (agent_name : string)
           (script : board_interface -> PuppetExternal.t -> unit) : region_agent_class =
  (** [create ?filter agent_name script] Creates a region agent class named [agent_name] that runs [script]
      when a puppet enters the region. The [filter] parameter controls which puppets trigger the script:
      - [PlayerOnly] (default): Only triggers for the player
      - [PuppetNamed name]: Only triggers for a puppet with the specified name
      - [AnyPuppet]: Triggers for any puppet entering the region *)

  let should_trigger (puppet : PuppetExternal.t) : bool =
    match filter with
    | PlayerOnly -> PuppetExternal.get_name puppet = "player"
    | PuppetNamed name -> PuppetExternal.get_name puppet = name
    | AnyPuppet -> true
  in

  let rec idle_state = {
    state_functions = {
      RegionAgentState.empty_state_functions with
        on_puppet_enter = Some(fun (board : board_interface) (puppet : PuppetExternal.t) () ->
          if should_trigger puppet then
            Some(RegionAgentState.create running_script_state puppet)
          else
            None
        )
    };

    props = {
      agent_set_invariant = StringSet.empty ;
      name = "Idle"
    }
  }
  and running_script_state = {
    state_functions = {
      RegionAgentState.empty_state_functions with
          script = Some (fun board puppet ->
            script board puppet;
          )
    };

    props = {
      agent_set_invariant = StringSet.empty ;
      name = "RunningScript"
    }
  } in
  {
    states = StringMap.of_list [
      ("Idle", idle_state.props) ;
      ("RunningScript", running_script_state.props)
    ];
    initial_state = RegionAgentState.create idle_state () ;
    speed = 1.0 ;
    name = agent_name
  }


