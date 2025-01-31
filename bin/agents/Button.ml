open Common
open Agent
open AgentState
open BoardInterface
open Channels

module Idle : AgentStateClass with type t_private_data = unit = struct
  type t_private_data = unit

  let state_functions = fun () -> {
    AgentState.empty_state_functions with
      receive_bump = Some(fun (board : board_interface) (me : Puppet.t) () (other : PuppetExternal.t) ->
        Channel.send_msg Buzzer.channel ();
        None
      )
  }

  let region_name = fun () -> None

  let name = fun () -> "Idle"
end

module Button : AgentClass = struct
  let states = StringMap.of_list [
    ("Idle", (module Idle : AgentStateClass)) ;
  ]

  let initial_state = AgentState.create (module Idle) ()

  let preview_texture_name = "person_south_recon.png"

  let preview_color = Raylib.Color.red

  let speed = 0.3

  let name = "button"
end

include Button