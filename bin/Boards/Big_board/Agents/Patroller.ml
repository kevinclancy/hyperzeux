open Common
open Agent
open AgentState
open BoardInterface

let name = "ruffian"

let walk_north (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "ruffian.png");
  Actions.walk_north ()

let walk_east (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "ruffian.png");
  Actions.walk_east ()

let walk_south (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "ruffian.png");
  Actions.walk_south ()

let walk_west (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "ruffian.png");
  Actions.walk_west ()

let walk_fns : Shared.AgentScriptFunctions.walk_functions = { walk_north ; walk_east ; walk_south; walk_west }

let acquire_kit =
  let open Shared.AgentStateCreators in
  AcquiredState.create Channels.ruffian_sees_greencup {
    walk_north ;
    walk_east ;
    walk_south ;
    walk_west ;
    walk_to = fun puppet board_intf waypoint ->
      Shared.AgentScriptFunctions.walk_to board_intf waypoint walk_fns puppet
  }

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
      );
      create_handlers = Some(fun () ->
        [
          acquire_kit.acquire_handler
        ]
      );
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
        let open Shared.AgentScriptFunctions in
        walk_to board "special_plant" walk_fns me
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

let () = acquire_kit.set_return_state (AgentState.create patrolling_state ())

let patroller_class : agent_class = {
  states = StringMap.of_list [
    ("Patrolling", patrolling_state.props) ;
    ("Freaking Out", freaking_out_state.props)
  ];
  initial_state = AgentState.create patrolling_state () ;
  preview_texture_name = "ruffian.png" ;
  preview_color = Raylib.Color.purple ;
  speed = 0.3 ;
  name
} |> acquire_kit.add_acquire_state