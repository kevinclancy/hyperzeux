open Common
open AgentState
open Agent
open BoardInterface

let create (agent_name : string)
           (texture_name : string)
           (color : Raylib.Color.t)
           (script : board_interface -> Puppet.t -> unit) : Agent.agent_class =
  (** [bump_bot agent_name texture_name color script] Creates an agent class named [agent_name], whose display image is named by
      [texture_name] and whose color is [color], and who performs [script] each time it receives a bump from the player.
      It will not restart the script if bumped while performing.  *)

  let rec idle_state = {
    state_functions = {
      AgentState.empty_state_functions with
        script = Some(fun (board : board_interface) (me : Puppet.t) () ->
          Puppet.set_texture me (TextureMap.get texture_name)
        );
        receive_bump = Some(fun (board : board_interface) (me : Puppet.t) () (other : PuppetExternal.t) ->
          Some(AgentState.create running_script_state ())
        )
    };

    props = {
      region_name = None ;
      name = "Idle"
    }
  }
  and running_script_state = {
    state_functions = {
      AgentState.empty_state_functions with
          (** TODO: change state to Idle at end of script *)
          script = Some (fun board puppet _ ->
            script board puppet;
            ScriptFunctions.set_state @@ AgentState.create idle_state ()
          )
    };

    props = {
      region_name = None ;
      name = "Running Script"
    }
  } in
  {
    states = StringMap.of_list [
      ("Idle", idle_state.props) ;
      ("Running Script", running_script_state.props)
    ];
    initial_state = AgentState.create idle_state () ;
    preview_texture_name = texture_name ;
    preview_color = color ;
    speed = 0.3 ;
    name = agent_name
  }


