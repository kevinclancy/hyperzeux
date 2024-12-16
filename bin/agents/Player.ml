open Common
open AgentClass_intf

let walk_north (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person2_north.png");
  Actions.walk_north ()

let walk_east (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person2_east.png");
  Actions.walk_east ()

let walk_south (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person2_south.png");
  Actions.walk_south ()

let walk_west (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person2_west.png");
  Actions.walk_west ()

let name = "player"

let preview_texture_name = "person2_south.png"

let preview_color = Raylib.Color.skyblue

let create (board : board_interface) (agent_name : string) (pos : position) (color : Raylib.Color.t) : Agent.t =
  let scripts = {
    Agent.empty_scripts with
      key_left = Some (function (me : Agent.t) -> walk_west me) ;
      key_right = Some (function (me : Agent.t) -> walk_east me) ;
      key_up = Some (function (me : Agent.t) -> walk_north me) ;
      key_down = Some (function (me : Agent.t) -> walk_south me)
  }
  in
  Agent.create agent_name scripts pos color (TextureMap.get "person2_south.png") ~speed:0.8
