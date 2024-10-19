open Common

module Blueprint = struct
  type agent_blueprint = {
    (* A key in the AgentClassMap identifying the AgentClass to use *)
    agent_class_name : string ;
    (* A unique name for the agent *)
    agent_name : string ;
    (* The starting position of the agent *)
    pos : position ;
    (* The name of the texture used to represent this agent in the level editor *)
    texture_name : string
  }

  type static_object_blueprint = {
    name : string ;
    color : Raylib.Color.t
  }

  type t = {
    (** Grid cells of board in width *)
    width : int ;
    (** Height of board in width *)
    height : int ;
    (* static_objects[x][y] is a key of the StaticObjectMap
        for the static object at position (x,y) *)
    static_objects : static_object_blueprint ref array array;
    (* For each agent in the board's starting state. *)
    agents : agent_blueprint list ref;
    (* A texture depicting the full static object map. *)
    static_bg_texture : Raylib.RenderTexture.t ;
    (* Texture to render to screen *)
    render_texture : Raylib.RenderTexture.t ;
    (* A translucent grid to optionally render over the level editor, letting the user
       see the cell boundaries. *)
    grid_texture : Raylib.RenderTexture.t
  }

  let create_empty (width : int) (height : int) (empty_object_key : string) : t =
    let open Raylib in
    let board_width = width * Config.char_width in
    let board_height = height * Config.char_height in
    let static_bg_texture = load_render_texture board_width board_height in
    begin_texture_mode static_bg_texture;
      draw_rectangle 0 0 board_width board_height Color.black;
    end_texture_mode ();
    let grid_texture = load_render_texture board_width board_height in
    begin_texture_mode grid_texture;
      for column = 0 to width do
        let x = column * Config.char_width in
        draw_line x 0 x board_height Config.grid_color
      done;
      for row = 0 to height do
        let y = row * Config.char_height in
        draw_line 0 y board_width y Config.grid_color
      done;
    end_texture_mode ();
    {
      width ;
      height ;
      static_objects =
        Array.init
          width
          (fun _ -> Array.init height (fun _ -> ref { name = empty_object_key ; color = Color.white }));
      agents = ref [] ;
      static_bg_texture ;
      render_texture =
        Raylib.load_render_texture
          (width * Config.char_width)
          (height * Config.char_height);
      grid_texture
    }

  let get_static_object_ref (bp : t) (pos : position) : static_object_blueprint ref =
    Array.get (Array.get bp.static_objects pos.x) pos.y

  let set_static_object (bp : t) (pos : position) (static_obj_key : string) (color : Raylib.Color.t): unit =
    get_static_object_ref bp pos := { name = static_obj_key; color };
    let static_obj = StaticObjectMap.get static_obj_key in
    let open Raylib in
    begin_texture_mode bp.static_bg_texture;
      let texture = TextureMap.get static_obj.texture_name in
      let x = Float.of_int @@ Config.char_width * pos.x in
      let y = Float.of_int @@ Config.char_height * pos.y in
      draw_texture_ex texture (Vector2.create x y) 0.0 1.0 color;
    end_texture_mode ()


  let remove_agent_at (bp : t) (pos : position) : unit =
    bp.agents := List.filter (fun agent_bp -> agent_bp.pos <> pos) !(bp.agents)

  let add_agent (bp : t) (agent_name : string) (agent_class_name : string) (texture_name : string) (pos : position) =
    assert (StaticObjectMap.get (!(get_static_object_ref bp pos)).name).traversable;
    remove_agent_at bp pos;
    bp.agents := { agent_class_name ; agent_name; texture_name; pos } :: !(bp.agents)

  let draw_prep (bp : t) : unit =
    let open Raylib in
    begin_texture_mode bp.render_texture;
      let draw_agent (agent_blueprint : agent_blueprint) : unit =
        let { x = x ; y = y } = agent_blueprint.pos in
        let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
        let texture = TextureMap.get agent_blueprint.texture_name in
        draw_texture_ex texture pos 0.0 1.0 Color.white;
      in
      let width = (Float.of_int Config.board_pixels_width) in
      let height = (Float.of_int Config.board_pixels_height) in
      let src = Rectangle.create 0.0 0.0 width (-. height) in
      let dest = Rectangle.create 0.0 0.0 width height in
      draw_texture_pro (RenderTexture.texture bp.static_bg_texture) src dest (Vector2.create 0.0 0.0) 0.0 Color.white;
      List.iter draw_agent !(bp.agents);
      draw_texture_pro (RenderTexture.texture bp.grid_texture) src dest (Vector2.zero ()) 0.0 Color.white;
    end_texture_mode ()

  (** [draw blueprint pos scale] *)
  let draw (bp : t) (pos : Raylib.Vector2.t) (scale : float) : unit =
    let open Raylib in
    clear_background Color.gray;
    let width = (Float.of_int Config.board_pixels_width) in
    let height = (Float.of_int Config.board_pixels_height) in
    let src = Rectangle.create 0.0 0.0 width (-. height) in
    let dest = Rectangle.create 0.0 0.0 (width *. scale) (height *. scale) in
    draw_texture_pro (RenderTexture.texture bp.render_texture) src dest pos 0.0 Color.white;
end

module AgentSet = Set.Make(Agent)

type grid_cell = {
  static_object : static_object ;
  static_object_color : Raylib.Color.t ;
  agent : Agent.t option ;
}

type t = {
  grid : grid_cell ref array array ;
  (* List of all agents *)
  agents : AgentSet.t ref;
  (* A texture depicting the full static object map *)
  static_texture : Raylib.RenderTexture.t ;
  (* Texture to render to screen *)
  render_texture : Raylib.RenderTexture.t ;
}

let create_empty (width : int)
                 (height : int)
                 (empty_object : static_object)  : t =
  let open Raylib in
  let static_texture =
    load_render_texture
      (width * Config.char_width)
      (height * Config.char_height)
  in
  begin_texture_mode static_texture;
  clear_background Color.black;
  end_texture_mode ();
  {
    grid =
      Array.init Config.board_cells_width (fun _ ->
        Array.init Config.board_cells_height (fun _ ->
          ref { static_object = empty_object ; static_object_color = Raylib.Color.white ; agent = None }
        )
      );
    agents = ref AgentSet.empty;
    static_texture ;
    render_texture =
      load_render_texture
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

let prep_draw (board : t) : unit =
  let open Raylib in
  begin_texture_mode board.render_texture;
    let draw_agent (agent : Agent.t) : unit =
      let {x;y} = Agent.get_pos agent in
      let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
      draw_texture_ex (Agent.get_texture agent) pos 0.0 1.0 Color.white;
    in
    draw_texture_ex (RenderTexture.texture board.static_texture) (Vector2.create 0.0 0.0) 0.0 1.0 Color.white;
    AgentSet.iter draw_agent !(board.agents);
  end_texture_mode ()

let draw (board : t) (pos : Raylib.Vector2.t) (scale : float) : unit =
  let open Raylib in
  clear_background Color.gray;
  let width = (Float.of_int Config.board_pixels_width) in
  let height = (Float.of_int Config.board_pixels_height) in
  let src = Rectangle.create 0.0 0.0 width (-. height) in
  let dest = Rectangle.create 0.0 0.0 (width *. scale) (height *. scale) in
  draw_texture_pro (RenderTexture.texture board.render_texture) src dest pos 0.0 Color.white

let update (board : t) : unit =
  let update_agent (agent : Agent.t) : unit =
    match Agent.resume agent with
    | Actions.Walk (delta_x, delta_y) ->
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
  AgentSet.iter update_agent !(board.agents)

let create_from_blueprint (blueprint : Blueprint.t) : t =
  let open Raylib in
  let create_static_obj (static_obj_bp : Blueprint.static_object_blueprint) : grid_cell ref =
    ref {
      static_object = StaticObjectMap.get static_obj_bp.name ;
      static_object_color = static_obj_bp.color;
      agent = None
    }
  in
  let grid = Array.map (fun a -> Array.map (fun bp -> create_static_obj !bp) a) blueprint.static_objects in
  let create_agent (agent_bp : Blueprint.agent_blueprint) : Agent.t =
    let open AgentClass_intf in
    let module ThisAgentClass = (val (AgentClassMap.get agent_bp.agent_class_name) : AgentClass) in
    ThisAgentClass.create agent_bp.agent_name agent_bp.pos
  in
  let agents = List.map create_agent !(blueprint.agents) in
  let add_agent (agent : Agent.t) : unit =
    let {x;y} = Agent.get_pos agent in
    let cell = Array.get (Array.get grid x) y in
    cell := { !(cell) with agent = Some agent }
  in
  List.iter add_agent agents;
  let board_width = blueprint.width * Config.char_width in
  let board_height = blueprint.height * Config.char_height in
  let static_texture = load_render_texture board_width board_height in
  begin_texture_mode static_texture;
    draw_rectangle 0 0 board_width board_height Color.black;
    for x = 0 to blueprint.width-1 do
      for y = 0 to blueprint.height-1 do
        let cell = Array.get (Array.get grid x) y in
        let src_rect = rect 0.0 0.0 (Float.of_int Config.char_width) (Float.of_int @@ - Config.char_height) in
        let dest_rect =
          rect
            (Float.of_int @@ x * Config.char_width)
            (Float.of_int @@ board_height - ((y + 1) * Config.char_height))
            (Float.of_int Config.char_width)
            (Float.of_int Config.char_height)
        in
        match (!cell).agent with
        | Some _ ->
          () (* agents are drawn per-frame *)
        | None ->
          let texture = TextureMap.get (!cell).static_object.texture_name in
          draw_texture_pro texture src_rect dest_rect (Vector2.zero ()) 1.0 (!cell).static_object_color;
      done;
    done;
  end_texture_mode ();
  {
    grid;
    agents = ref (AgentSet.of_list agents);
    static_texture;
    render_texture = load_render_texture board_width board_height
  }