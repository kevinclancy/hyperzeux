open Common
open Agent
open AgentState
open BoardInterface
open Channels

let idle_state = {
  state_functions = {
    AgentState.empty_state_functions with
      receive_bump = Some(fun (board : board_interface) (me : Puppet.t) () (other : PuppetExternal.t) ->
        Channel.send_msg Buzzer.channel ();
        None
      )
  };

  props = {
    region_name = None ;
    name = "Idle"
  }
}

module Button : AgentClass = struct
  let states = StringMap.of_list [
    ("Idle", idle_state.props) ;
  ]

  let initial_state = AgentState.create idle_state ()

  let preview_texture_name = "person_south_recon.png"

  let preview_color = Raylib.Color.red

  let speed = 0.3

  let name = "button"
end

include Button