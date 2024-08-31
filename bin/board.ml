open Common

module AgentSet = Set.Make(Agent)

type grid_cell = {
  static_object : static_object ;
  agent : Agent.t option ;
}

type t = {
  grid : grid_cell ref array array ;
  (* List of all agents *)
  agents : AgentSet.t ref;
  (* A texture depicting the full static object map *)
  static_texture : Raylib.Texture.t ;
  (* Texture to render to screen *)
  render_texture : Raylib.RenderTexture.t ;
}

let create_empty (width : int)
                 (height : int)
                 (empty_object : static_object)  : t =
  let imBlank =
    Raylib.gen_image_color
      (width * Config.char_width)
      (height * Config.char_height)
      Raylib.Color.black
  in
  let texture = Raylib.load_texture_from_image imBlank in
  {
    grid =
      Array.init Config.board_cells_width (fun _ ->
        Array.init Config.board_cells_height (fun _ ->
          ref { static_object = empty_object ; agent = None }
        )
      );
    agents = ref AgentSet.empty;
    static_texture = texture ;
    render_texture =
      Raylib.load_render_texture
        (width * Config.char_width)
        (height * Config.char_height);
  }

let get_cell (board : t) (pos : position) : grid_cell =
  !(Array.get (Array.get board.grid pos.x) pos.y)

let set_static_object (board : t) (pos : position) (static_object : static_object) : unit =
  let cell = (Array.get (Array.get board.grid pos.x) pos.y) in
  cell := { !cell with static_object }

let set_agent (board : t) (pos : position) (agent : Agent.t option) : unit =
  let cell = (Array.get (Array.get board.grid pos.x) pos.y) in
  cell := { !cell with agent = agent }

let is_occupied (board : t) (pos : position) =
  ((get_cell board pos).static_object.traversable = false)
  || (Option.is_some (get_cell board pos).agent)

let add_agent (board : t) (agent : Agent.t) : unit =
  assert (not @@ is_occupied board (Agent.get_pos agent));
  assert (not @@ AgentSet.mem agent !(board.agents));
  board.agents := AgentSet.add agent !(board.agents)

let draw (board : t) (pos : Raylib.Vector2.t) (scale : float) : unit =
  let open Raylib in
  begin_texture_mode board.render_texture;
    let draw_agent (agent : Agent.t) : unit =
      let { x = x ; y = y } = Agent.get_pos agent in
      let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
      draw_texture_ex (Agent.get_texture agent) pos 0.0 1.0 Color.white;
    in
    draw_texture_ex board.static_texture (Vector2.create 0.0 0.0) 0.0 1.0 Color.white;
    AgentSet.iter draw_agent !(board.agents);
  end_texture_mode ();

  begin_drawing ();
    clear_background Color.gray;
    let width = (Float.of_int Config.board_pixels_width) in
    let height = (Float.of_int Config.board_pixels_height) in
    let src = Rectangle.create 0.0 0.0 width (-. height) in
    let dest = Rectangle.create 0.0 0.0 (width *. scale) (height *. scale) in
    draw_texture_pro (RenderTexture.texture board.render_texture) src dest pos 0.0 Color.white;
  end_drawing ()

let update (board : t) : unit =
  let update_agent (agent : Agent.t) : unit =
    match Agent.resume agent with
    | Actions.Walk (delta_x, delta_y) ->
      Printf.printf "walk %d %d" delta_x delta_y;
      let pos = Agent.get_pos agent in
      let pos' = {x = pos.x + delta_x ; y = pos.y + delta_y} in
      if is_occupied board pos' then
        ()
      else
        begin
          set_agent board pos None;
          set_agent board pos' (Some agent);
          Agent.set_pos agent pos'
        end
    | Actions.Wait ->
      ()
  in
  AgentSet.iter update_agent !(board.agents);;

ignore set_agent;
(* let grid = Array.init Config.board_cells_width (fun _ ->
    Array.init Config.board_cells_height (fun _ ->
      ref { static_object = None ; agent = None }
    )
  ) *)