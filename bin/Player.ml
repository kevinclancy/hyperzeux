open Common

let walk_north (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_north_recon.png");
  Actions.walk_north ()

let walk_east (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_east_recon.png");
  Actions.walk_east ()

let walk_south (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_south_recon.png");
  Actions.walk_south ()

let walk_west (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_west_recon.png");
  Actions.walk_west ()

let name = "player"

let preview_texture_name = "person_south_recon.png"

let preview_color = Raylib.Color.skyblue

let create (agent_name : string) (pos : position) (color : Raylib.Color.t) : Agent.t =
  let scripts = {
    Agent.empty_scripts with
      key_left = Some (function (me : Agent.t) -> walk_west me) ;
      key_right = Some (function (me : Agent.t) -> walk_east me) ;
      key_up = Some (function (me : Agent.t) -> walk_north me) ;
      key_down = Some (function (me : Agent.t) -> walk_south me)
  }
  in
  Agent.create agent_name scripts pos color (TextureMap.get "person_south_recon.png") ~speed:0.8
