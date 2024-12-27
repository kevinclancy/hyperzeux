open Common
open Agent
open AgentState

let walk_north (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_north.png");
  Actions.walk_north ()

let walk_east (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_east.png");
  Actions.walk_east ()

let walk_south (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_south.png");
  Actions.walk_south ()

let walk_west (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_west.png");
  Actions.walk_west ()

module rec Controlled : AgentStateClass with type t_private_data = unit = struct
  (** Player is being controlled by arrow keys *)

  type t_private_data = unit

  let state_functions = fun () -> {
    AgentState.empty_state_functions with
      (* Initially start patrolling in a 2x2 square *)
      script = Some (fun (me : Puppet.t) () ->
        while true do
          walk_north me;
          walk_south me;
        done;
      )
  }

  (** Functions for interacting with agents in this state, where 't is the agent type *)

  let region_name = fun () -> "control_area"

  let name = fun () -> "Controlled"
end

module Player : AgentClass = struct
  let states = StringMap.of_list [
    ("Controlled", (module Controlled : AgentStateClass))
  ]

  let initial_state = (module Controlled : AgentStateClass with type t_private_data = unit)

  let preview_texture_name = "person2_south.png"

  let preview_color = Raylib.Color.skyblue

  let speed = 0.8

  let name = "Player"
end

include Player
