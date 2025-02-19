open Common
open Actions
open Agent
open AgentState
open BoardInterface
open Channels

let say (msg : string) : unit =
  let finished = ref false in
  Channel.send_msg Channels.Common.speech (msg, finished);
  while not !finished do
    ignore (Effect.perform @@ Act Wait)
  done

let rec idle_state : unit AgentState.blueprint = {
  state_functions = {
    AgentState.empty_state_functions with
      receive_bump = Some(fun (board : board_interface) (me : Puppet.t) () (other : PuppetExternal.t) ->
        Some(AgentState.create talking_state ())
      )
  };

  props = {
    region_name = None ;
    name = "Idle"
  }
}

and talking_state : unit AgentState.blueprint = {
  state_functions = {
    AgentState.empty_state_functions with
      script = Some(fun (board : board_interface) (me : Puppet.t) () ->
        say "hello world";
        say "goodbye world";
      )
  };

  props = {
    region_name = None ;
    name = "Talking"
  }
}

let button_class : agent_class = {
  states = StringMap.of_list [
    ("Idle", idle_state.props) ;
    ("Talking", talking_state.props) ;
  ];

  initial_state = AgentState.create idle_state ();

  preview_texture_name = "person_south_recon.png";

  preview_color = Raylib.Color.red;

  speed = 0.3;

  name = "button"
}