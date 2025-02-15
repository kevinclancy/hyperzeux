open Common
open Region
open BoardInterface

let region_colors =
  Array.map
    (fun c -> Raylib.color_alpha c 0.12)
    Raylib.[| Color.blue ; Color.brown ; Color.green ; Color.magenta ; Color.gold ; Color.purple ; Color.orange ; Color.pink |]

module Blueprint = struct
  type agent_blueprint = {
    agent_class_name : string ;
    (** A key in the AgentClassMap identifying the AgentClass to use *)
    agent_name : string ;
    (** A unique name for the agent *)
    color : Raylib.Color.t ;
    (** The starting color of this agent *)
    pos : position ;
    (** The starting position of the agent *)
    texture_name : string
    (** The name of the texture used to represent this agent in the level editor *)
  }

  type static_object_blueprint = {
    name : string ;
    color : Raylib.Color.t
  }

  type waypoint = {
    name : string ;
    (** the name of the waypoint *)

    position : position ;
    (** the position of the waypoint *)
  }

  (** Maps each component name to the component *)
  type t = {
    width : int ;
    (** Grid cells of board in width *)

    height : int ;
    (** Height of board in width *)

    static_objects : static_object_blueprint ref array array ;
    (** static_objects[x][y] is a key of the StaticObjectMap
        for the static object at position (x,y) *)

    mutable agents : agent_blueprint StringMap.t ;
    (** For each agent in the board's starting state, a blueprint. *)

    mutable waypoints : position StringMap.t ;
    (** Maps waypoint names to waypoint positions *)

    mutable regions : region StringMap.t ;
    (** Maps each region name to the region *)

    static_bg_texture : Raylib.RenderTexture.t ;
    (** A texture depicting the full static object map. *)

    render_texture : Raylib.RenderTexture.t ;
    (** Texture to render to screen *)

    grid_texture : Raylib.RenderTexture.t
    (** A translucent grid to optionally render over the level editor, letting the user
       see the cell boundaries. *)
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
      agents = StringMap.empty ;
      regions = StringMap.empty ;
      static_bg_texture ;
      render_texture =
        Raylib.load_render_texture
          (width * Config.char_width)
          (height * Config.char_height);
      grid_texture ;
      waypoints = StringMap.empty
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
    bp.agents <- StringMap.filter (fun _ agent_bp -> agent_bp.pos <> pos) bp.agents

  let remove_waypoint_at (bp : t) (pos : position) : unit =
    bp.waypoints <- StringMap.filter (fun _ waypoint_pos -> waypoint_pos <> pos) bp.waypoints

  let add_agent (bp : t) (agent_name : string) (color : Raylib.Color.t) (agent_class_name : string) (texture_name : string) (pos : position) =
    assert (StaticObjectMap.get (!(get_static_object_ref bp pos)).name).traversable;
    assert (not @@ StringMap.mem agent_name bp.agents);
    remove_agent_at bp pos;
    bp.agents <- StringMap.add agent_name { agent_class_name ; agent_name; color; texture_name; pos } bp.agents

  let contains_waypoint_name (bp : t) (waypoint_name : string) : bool =
    StringMap.mem waypoint_name bp.waypoints

  let contains_agent_name (bp : t) (agent_name : string) : bool =
    StringMap.mem agent_name bp.agents

  let region_names (bp : t) : string list =
    List.map fst (StringMap.to_list bp.regions)

  let region (bp : t) (name : string) : region =
    StringMap.find name bp.regions

  let add_region (bp : t) (name :string) (region : region) =
    bp.regions <- StringMap.add name region bp.regions

  let add_waypoint (bp : t) (waypoint_name : string) (pos : position) : unit =
    assert (StaticObjectMap.get (!(get_static_object_ref bp pos)).name).traversable;
    assert (not @@ contains_waypoint_name bp waypoint_name);
    remove_waypoint_at bp pos;
    bp.waypoints <- StringMap.add waypoint_name pos bp.waypoints

  let draw_prep (bp : t) : unit =
    let open Raylib in
    begin_texture_mode bp.render_texture;
      let draw_agent (_ : string) (agent_blueprint : agent_blueprint) : unit =
        let { x ; y } = agent_blueprint.pos in
        let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
        let texture = TextureMap.get agent_blueprint.texture_name in
        draw_texture_ex texture pos 0.0 1.0 agent_blueprint.color;
      in
      let draw_waypoint (_ : string) (waypoint : position) : unit =
        let { x ; y } = waypoint in
        let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
        let texture = TextureMap.get "waypoint.png" in
        draw_texture_ex texture pos 0.0 1.0 Raylib.Color.green
      in
      let draw_region_component (color : Raylib.Color.t) (_ : string) (component : region_component) : unit =
        let width = component.right - component.left + 1 in
        let height = component.bottom - component.top + 1 in
        draw_rectangle
          (component.left * Config.char_width)
          (component.top * Config.char_height)
          (width * Config.char_width)
          (height * Config.char_height)
          color
      in
      let region_color_ind = ref 0 in
      let draw_region (_ : string) (region : region) : unit =
        StringMap.iter (draw_region_component @@ Array.get region_colors !region_color_ind) region.components;
        region_color_ind := (!region_color_ind + 1) mod (Array.length region_colors)
      in
      let width = (Float.of_int Config.board_pixels_width) in
      let height = (Float.of_int Config.board_pixels_height) in
      let src = Rectangle.create 0.0 0.0 width (-. height) in
      let dest = Rectangle.create 0.0 0.0 width height in
      draw_texture_pro (RenderTexture.texture bp.static_bg_texture) src dest (Vector2.create 0.0 0.0) 0.0 Color.white;
      StringMap.iter draw_agent bp.agents;
      StringMap.iter draw_waypoint bp.waypoints;
      StringMap.iter draw_region bp.regions;
      draw_texture_pro (RenderTexture.texture bp.grid_texture) src dest (Vector2.zero ()) 0.0 Color.white;
    end_texture_mode ()

  let output_binary_string (chan : out_channel) (s : string) : unit =
    output_binary_int chan (String.length s);
    output_bytes chan (String.to_bytes s)

  let input_binary_string (chan : in_channel) : string =
    let len = input_binary_int chan in
    really_input_string chan len

  let output_static_obj (chan : out_channel) (obj : static_object_blueprint) : unit =
    output_binary_string chan obj.name;
    output_binary_int chan (Raylib.Color.r obj.color);
    output_binary_int chan (Raylib.Color.g obj.color);
    output_binary_int chan (Raylib.Color.b obj.color);
    output_binary_int chan (Raylib.Color.a obj.color)

  let input_static_obj (chan : in_channel) : static_object_blueprint =
    let name = input_binary_string chan in
    let r = input_binary_int chan in
    let g = input_binary_int chan in
    let b = input_binary_int chan in
    let a = input_binary_int chan in
    { name ; color = Raylib.Color.create r g b a }

  let output_agent (chan : out_channel) (agent_name : string) (agent : agent_blueprint) : unit =
    output_binary_string chan agent.agent_class_name;
    output_binary_string chan agent.agent_name;
    output_binary_int chan agent.pos.x;
    output_binary_int chan agent.pos.y;
    output_binary_string chan agent.texture_name;
    output_binary_int chan (Raylib.Color.r agent.color);
    output_binary_int chan (Raylib.Color.g agent.color);
    output_binary_int chan (Raylib.Color.b agent.color);
    output_binary_int chan (Raylib.Color.a agent.color)

  let output_region_component (chan : out_channel) (component_name : string) (component : region_component) : unit =
    output_binary_string chan component_name;
    output_binary_int chan component.top;
    output_binary_int chan component.left;
    output_binary_int chan component.bottom;
    output_binary_int chan component.right

  let input_region_component (chan : in_channel) : string * region_component =
    (** Returns pair of (region component name, region component) *)
    let name = input_binary_string chan in
    let top = input_binary_int chan in
    let left = input_binary_int chan in
    let bottom = input_binary_int chan in
    let right = input_binary_int chan in
    let region_component = { top ; left ; bottom ; right } in
    name, region_component

  let output_region (chan : out_channel) (region_name : string) (region : region) : unit =
    output_binary_string chan region_name;
    output_binary_string chan region.description;
    output_binary_int chan (StringMap.cardinal region.components);
    StringMap.iter (output_region_component chan) region.components

  let input_region (chan : in_channel) : string * region =
    (** Returns pair of (region name, region) *)
    let region_name = input_binary_string chan in
    let description = input_binary_string chan in
    let num_components = input_binary_int chan in
    let components = ref StringMap.empty in
    for i = 0 to (num_components-1) do
      let name, component = input_region_component chan in
      components := StringMap.add name component (!components);
    done;
    region_name, { description ; components = !components }

  let input_agent (chan : in_channel) : agent_blueprint =
    let agent_class_name = input_binary_string chan in
    let agent_name = input_binary_string chan in
    let x = input_binary_int chan in
    let y = input_binary_int chan in
    let texture_name = input_binary_string chan in
    let r = input_binary_int chan in
    let g = input_binary_int chan in
    let b = input_binary_int chan in
    let a = input_binary_int chan in
    {
      agent_class_name ;
      agent_name ;
      color = Raylib.Color.create r g b a;
      pos = {x;y} ;
      texture_name
    }

  let serialize (bp : t) (filename : string) : unit =
    let chan = open_out_bin filename in
    output_binary_int chan bp.width;
    output_binary_int chan bp.height;
    for x = 0 to bp.width - 1 do
      for y = 0 to bp.height - 1 do
        output_static_obj chan !(get_static_object_ref bp {x; y})
      done;
    done;
    output_binary_int chan (StringMap.cardinal bp.agents);
    StringMap.iter (output_agent chan) bp.agents;
    output_binary_int chan (StringMap.cardinal bp.regions);
    StringMap.iter (output_region chan) bp.regions;
    close_out chan

  let deserialize (filename : string) : t =
    let chan = open_in_bin filename in
    let width = input_binary_int chan in
    let height = input_binary_int chan in
    let result_bp = create_empty width height "empty" in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        let obj = input_static_obj chan in
        let { name ; color } = obj in
        set_static_object result_bp {x ; y} name color;
      done;
    done;
    let num_agents = input_binary_int chan in
    for i = 0 to num_agents - 1 do
      let agent = input_agent chan in
      add_agent result_bp agent.agent_name agent.color agent.agent_class_name agent.texture_name agent.pos;
    done;
    let num_regions = input_binary_int chan in
    for i = 0 to num_regions - 1 do
      let region_name, region = input_region chan in
      result_bp.regions <- StringMap.add region_name region result_bp.regions;
    done;
    close_in chan;
    draw_prep result_bp;
    result_bp

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

type grid_cell = {
  static_object : static_object ;
  static_object_color : Raylib.Color.t ;
  agent : Agent.t option ;
}

type t = {
  grid : grid_cell ref array array ;

  mutable agents : (Agent.t * (Actions.action_result ref)) StringMap.t;
  (** Maps each agent name to agent, paired with result of last action the agent yielded to board
      (or Success if the agent has not yet performed an action). *)

  mutable waypoints : position StringMap.t ;
  (** Maps each waypoint name to position *)

  static_texture : Raylib.RenderTexture.t ;
  (** A texture depicting the full static object map *)

  render_texture : Raylib.RenderTexture.t ;
  (** Texture to render to screen *)
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
    agents = StringMap.empty;
    waypoints = StringMap.empty;
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
  assert (not @@ is_occupied board (Agent.position agent));
  assert (not @@ StringMap.mem (Agent.name agent) board.agents);
  board.agents <- StringMap.add (Agent.name agent) (agent, ref Actions.Success) board.agents

let prep_draw (board : t) : unit =
  let open Raylib in
  begin_texture_mode board.render_texture;
    let draw_agent (_ : string) ((agent,_) : Agent.t * Actions.action_result ref) : unit =
      let {x;y} = Agent.position agent in
      let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
      draw_texture_ex (Agent.texture agent) pos 0.0 1.0 (Agent.color agent);
    in
    draw_texture_ex (RenderTexture.texture board.static_texture) (Vector2.create 0.0 0.0) 0.0 1.0 Color.white;
    StringMap.iter draw_agent board.agents;
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
  let open Raylib in
  let update_agent (_ : string) ((agent, prev_result) : Agent.t * Actions.action_result ref) : unit =
    Agent.handle_messages agent;
    Agent.update_input agent;
    match Agent.resume agent !prev_result with
    | Actions.Walk (delta_x, delta_y) ->
      let pos = Agent.position agent in
      let pos' = {x = pos.x + delta_x ; y = pos.y + delta_y} in
      begin
        match is_occupied board pos' with
        | false ->
          set_agent board pos None;
          set_agent board pos' (Some agent);
          Agent.set_position agent pos';
          prev_result := Success
        | true ->
          prev_result := Failure;
          let occupied_cell = get_cell board pos' in
          match occupied_cell.agent with
          | Some(bump_target) ->
            let puppet = Agent.puppet agent in
            Agent.receive_bump bump_target (PuppetExternal.from puppet)
          | None ->
            ()
      end
    | Actions.Wait ->
      prev_result := Success
    | Actions.Idle ->
      (* the result of the previous action hasn't been returned to the agent yet, so keep it *)
      ()
  in
  StringMap.iter update_agent board.agents

let create_from_blueprint (blueprint : Blueprint.t) : t =
  let open Raylib in
  let open Agent in
  let create_static_obj (static_obj_bp : Blueprint.static_object_blueprint) : grid_cell ref =
    ref {
      static_object = StaticObjectMap.get static_obj_bp.name ;
      static_object_color = static_obj_bp.color;
      agent = None
    }
  in
  let grid = Array.map (fun a -> Array.map (fun bp -> create_static_obj !bp) a) blueprint.static_objects in
  let board_intf = {
      get_waypoint = (fun (name : string) ->
        StringMap.find name blueprint.waypoints
      );
      get_region = (fun (name : string) ->
        StringMap.find name blueprint.regions
      )
  } in
  let create_agent (agent_bp : Blueprint.agent_blueprint) : Agent.t =
    let agent_class = AgentClassMap.get agent_bp.agent_class_name in
    Agent.create board_intf agent_class agent_bp.agent_name agent_bp.pos agent_bp.color
  in
  let agents = StringMap.map (fun x -> (create_agent x, ref Actions.Success)) blueprint.agents in
  let add_agent (_ : string) ((agent, _) : Agent.t * Actions.action_result ref) : unit =
    let {x;y} = Agent.position agent in
    let cell = Array.get (Array.get grid x) y in
    cell := { !(cell) with agent = Some agent }
  in
  StringMap.iter add_agent agents;
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
    agents = agents;
    waypoints = blueprint.waypoints;
    static_texture;
    render_texture = load_render_texture board_width board_height
  }