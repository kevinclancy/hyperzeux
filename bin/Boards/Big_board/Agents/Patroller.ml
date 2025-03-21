open Common
open Agent
open AgentState
open BoardInterface

let walk_north (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person_north_recon.png");
  Actions.walk_north ()

let walk_east (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person_east_recon.png");
  Actions.walk_east ()

let walk_south (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person_south_recon.png");
  Actions.walk_south ()

let walk_west (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person_west_recon.png");
  Actions.walk_west ()

let rec patrolling_state : unit AgentState.blueprint = {
  state_functions = {
    AgentState.empty_state_functions with
      script = Some (fun (board : board_interface) (me : Puppet.t) () ->
        while true do
          walk_north me;
          walk_north me;

          walk_east me;
          walk_east me;

          walk_south me;
          walk_south me;

          walk_west me;
          walk_west me;
        done;
      );
      receive_bump = Some(fun (board : board_interface) (me : Puppet.t) () (other : PuppetExternal.t) ->
        Some(AgentState.create freaking_out_state ())
      )
  };

  props = {
    region_name = None ;
    name = "Patrolling"
  }
}

and freaking_out_state : unit AgentState.blueprint = {
  state_functions = {
    AgentState.empty_state_functions with
      script = Some (fun (board : board_interface) (me : Puppet.t) () ->
        while true do
          walk_north me;
          walk_south me;
          walk_south me;
        done;
      );
      key_up_pressed = Some(fun (board : board_interface) (me : Puppet.t) () ->
        Some(AgentState.create patrolling_state ())
      )
  };

  props = {
    region_name = None ;
    name = "Freaking Out"
  }
}

let patroller_class : agent_class = {
  states = StringMap.of_list [
    ("Patrolling", patrolling_state.props) ;
    ("Freaking Out", freaking_out_state.props)
  ];
  initial_state = AgentState.create patrolling_state () ;
  preview_texture_name = "person_south_recon.png" ;
  preview_color = Raylib.Color.white ;
  speed = 0.3 ;
  name = "patroller"
}