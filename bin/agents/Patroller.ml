open Common
open Agent
open AgentState

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

let name = "patroller"

let preview_texture_name = "person_south_recon.png"

let preview_color = Raylib.Color.white

module rec Patrolling : AgentStateClass with type t_private_data = unit = struct
  type t_private_data = unit

  let state_functions = fun () -> {
    AgentState.empty_state_functions with
      (* Initially start patrolling in a 2x2 square *)
      script = Some (fun (me : Puppet.t) () ->
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
      key_up_pressed = Some(fun (me : Puppet.t) () ->
        Some(AgentState.create (module FreakingOut) ())
      )
  }

  let region_name = fun () -> Some "patrol_area"

  let name = fun () -> "Patrolling"
end
and FreakingOut : AgentStateClass with type t_private_data = unit = struct
  type t_private_data = unit

  let state_functions = fun () -> {
    AgentState.empty_state_functions with
      (* Initially start patrolling in a 2x2 square *)
      script = Some (fun (me : Puppet.t) () ->
        while true do
          walk_north me;
          walk_south me;
        done;
      );
      key_up_pressed = Some(fun (me : Puppet.t) () ->
        Some(AgentState.create (module Patrolling) ())
      )
  }

  let region_name = fun () -> Some "freak_out_area"

  let name = fun () -> "Freaking Out"
end

module Patroller : AgentClass = struct
  let states = StringMap.of_list [
    ("Patrolling", (module Patrolling : AgentStateClass))
  ]

  let initial_state = AgentState.create (module Patrolling) ()

  let preview_texture_name = "person_south_recon.png"

  let preview_color = Raylib.Color.white

  let speed = 0.3

  let name = "Patroller"
end

include Patroller

(* let create (board_intf : board_interface) (agent_name : string) (pos : position) (color : Raylib.Color.t) : Agent.t =
  Agent.create
    board_intf
    (module Patroller)
    (module Patrolling)
    agent_name
    ~speed:0.3
    pos
    color *)
