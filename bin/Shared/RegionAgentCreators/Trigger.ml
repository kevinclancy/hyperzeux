open Common
open RegionAgentState
open RegionAgent
open BoardInterface

let create (agent_name : string)
           (script : board_interface -> PuppetExternal.t -> unit) : region_agent_class =
  (** [bump_bot agent_name texture_name color script] Creates an agent class named [agent_name], whose display image is named by
      [texture_name] and whose color is [color], and who performs [script] each time it receives a bump from the player.
      It will not restart the script if bumped while performing.  *)

  let rec idle_state = {
    state_functions = {
      RegionAgentState.empty_state_functions with
        on_puppet_enter = Some(fun (board : board_interface) (puppet : PuppetExternal.t) () ->
          Some(RegionAgentState.create running_script_state puppet)
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


