open Common
open Region
open BoardInterface

let region_colors =
  Array.map
    (fun c -> Raylib.color_alpha c 0.12)
    Raylib.[| Color.blue ; Color.brown ; Color.green ; Color.magenta ; Color.gold ; Color.purple ; Color.orange ; Color.pink |]

module Blueprint = struct
  type ambient_agent_blueprint = {
    agent_class_name : string ;
    (** Name of the ambient agent class *)
    agent_name : string
    (** A unique name for the agent *)
  }

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

  type layer = {
    width : int ;
    (** Width of board in grid cells *)

    height : int ;
    (** Height of board in grid cells *)

    static_objects : static_object_blueprint ref array array ;
    (** static_object_grid[x][y] is a color and key of the StaticObjectMap
        for the static object at position (x,y) in this layer *)

    static_bg_texture : Raylib.RenderTexture.t ;
    (** A texture depicting the full static object grid. *)

    grid_texture : Raylib.RenderTexture.t
    (** A translucent grid to optionally render over the level editor, letting the user
       see the cell boundaries. *)
  }

  type t = {
    mutable layers : layer StringMap.t;

    mutable agents : agent_blueprint StringMap.t ;
    (** For each agent in the board's starting state, maps agent name to a blueprint. *)

    mutable ambient_agents : ambient_agent_blueprint StringMap.t ;
    (** For each ambient agent in the board's starting state, maps ambient agent name to a blueprint *)

    mutable waypoints : position StringMap.t ;
    (** Maps waypoint names to waypoint positions *)

    mutable regions : region StringMap.t ;
    (** Maps each region name to the region *)
  }

  type edit_state = {
    blueprint : t ;

    mutable current_layer : string ;
    (** The layer we are currently editing *)

    mutable render_texture : Raylib.RenderTexture.t ;
    (** Texture to render to screen *)
  }

  let get_current_layer (s : edit_state) : layer =
    StringMap.find s.current_layer s.blueprint.layers

  let get_width (s : edit_state) : int =
    (get_current_layer s).width

  let get_height (s : edit_state) : int =
    (get_current_layer s).height

  let get_blueprint (s : edit_state) : t =
    s.blueprint

  let get_layer_names (s : edit_state) : string list =
    List.map fst (StringMap.to_list s.blueprint.layers)

  let get_current_layer_name (s : edit_state) : string =
    s.current_layer

  let create_empty_layer (width : int) (height : int) (empty_object_key : string) : layer =
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
      width;
      height;
      static_objects =
        Array.init
          width
          (fun _ -> Array.init height (fun _ -> ref { name = empty_object_key ; color = Color.white }));
      static_bg_texture;
      grid_texture
    }

  let create_empty () : t =
    {
      layers = StringMap.empty ;
      agents = StringMap.empty ;
      ambient_agents = StringMap.empty ;
      waypoints = StringMap.empty ;
      regions = StringMap.empty ;
    }

  let create_initial_state (width : int) (height : int) (empty_object_key : string) : edit_state =
    let open Raylib in
    let main_layer = create_empty_layer width height empty_object_key in
    let init_bp =
      { (create_empty ()) with
        layers = StringMap.of_list [("main", main_layer)]
      }
    in
    {
      current_layer = "main" ;
      blueprint = init_bp ;
      render_texture =
        Raylib.load_render_texture
          (width * Config.char_width)
          (height * Config.char_height);
    }

  let add_layer (s : edit_state) (name : string) (width : int) (height : int) =
    let layer = create_empty_layer width height "empty" in
    s.blueprint.layers <- (StringMap.add name layer s.blueprint.layers)

  let select_layer (s : edit_state) (name : string) =
    s.current_layer <- name;
    let current_layer = StringMap.find name s.blueprint.layers in
    s.render_texture <-
      Raylib.load_render_texture
        (current_layer.width * Config.char_width)
        (current_layer.height * Config.char_height)

  let get_static_object_ref (bp : t) (pos : position) : static_object_blueprint ref =
    let layer = StringMap.find pos.layer bp.layers in
    Array.get (Array.get layer.static_objects pos.x) pos.y

  let static_object_from_layer (layer : layer) (pos : pre_position) : static_object_blueprint ref =
    Array.get (Array.get layer.static_objects pos.x) pos.y

  let set_static_object_l (layer : layer) (pos : pre_position) (static_obj_key : string) (color : Raylib.Color.t) : unit =
  (** Set the static object a pre-position [pos] in layer [layer]. *)
    static_object_from_layer layer pos := { name = static_obj_key; color };
    let static_obj = StaticObjectMap.get static_obj_key in
    let open Raylib in
    begin_texture_mode layer.static_bg_texture;
      let texture = TextureMap.get static_obj.texture_name in
      let x = Float.of_int @@ Config.char_width * pos.x in
      let y = Float.of_int @@ Config.char_height * pos.y in
      draw_texture_ex texture (Vector2.create x y) 0.0 1.0 color;
    end_texture_mode ()

  let set_static_object (s : edit_state) (pos : pre_position) (static_obj_key : string) (color : Raylib.Color.t): unit =
    let layer = StringMap.find s.current_layer s.blueprint.layers in
    set_static_object_l layer {x=pos.x ; y=pos.y} static_obj_key color

  let get_static_object_name (s : edit_state) (pos : pre_position) : string =
    !(get_static_object_ref s.blueprint { layer = s.current_layer ; x = pos.x ; y = pos.y }).name

  let remove_agent_at (bp : t) (pos : position) : unit =
    bp.agents <- StringMap.filter (fun _ agent_bp -> agent_bp.pos <> pos) bp.agents

  let remove_waypoint_at (bp : t) (pos : position) : unit =
    bp.waypoints <- StringMap.filter (fun _ waypoint_pos -> waypoint_pos <> pos) bp.waypoints

  let add_agent_bp (bp : t) (agent_name : string) (color : Raylib.Color.t) (agent_class_name : string) (texture_name : string) (pos : position) =
    assert (StaticObjectMap.get (!(get_static_object_ref bp pos)).name).traversable;
    assert (not @@ StringMap.mem agent_name bp.agents);
    remove_agent_at bp pos;
    bp.agents <- StringMap.add agent_name { agent_class_name ; agent_name; color; texture_name; pos } bp.agents

  let add_agent (s : edit_state) (agent_name : string) (color : Raylib.Color.t) (agent_class_name : string) (texture_name : string) (pos : pre_position) : unit =
    add_agent_bp s.blueprint agent_name color agent_class_name texture_name {layer=s.current_layer; x=pos.x ; y=pos.y}

  let add_ambient_agent_bp (bp : t) (agent_name : string) (agent_class_name : string) =
    assert (not @@ StringMap.mem agent_name bp.ambient_agents);
    bp.ambient_agents <- StringMap.add agent_name { agent_class_name ; agent_name } bp.ambient_agents

  let add_ambient_agent (s : edit_state) (agent_name : string) (agent_class_name : string) =
    add_ambient_agent_bp s.blueprint agent_name agent_class_name

  let remove_ambient_agent (bp : t) (agent_name : string) : unit =
    assert (StringMap.mem agent_name bp.ambient_agents);
    bp.ambient_agents <- StringMap.remove agent_name bp.ambient_agents

  let contains_pos (s : edit_state) (pos : pre_position) : bool =
    let layer = StringMap.find s.current_layer s.blueprint.layers in
    pos.x >= 0 && pos.x < layer.width && pos.y >= 0 && pos.y < layer.height

  let contains_waypoint_name (s : edit_state) (waypoint_name : string) : bool =
    StringMap.mem waypoint_name s.blueprint.waypoints

  let contains_agent_name (edit_state : edit_state) (agent_name : string) : bool =
    StringMap.mem agent_name edit_state.blueprint.agents

  let contains_ambient_name (bp : t) (agent_name : string) : bool =
    StringMap.mem agent_name bp.ambient_agents

  let region_names (s : edit_state) : string list =
    List.map fst (StringMap.to_list s.blueprint.regions)

  let ambient_names (s : edit_state) : string list =
    List.map fst (StringMap.to_list s.blueprint.ambient_agents)

  let get_region (s : edit_state) (name : string) : region =
    StringMap.find name s.blueprint.regions

  let add_region (s : edit_state) (name :string) (region : region) =
    s.blueprint.regions <- StringMap.add name region s.blueprint.regions

  let add_waypoint (s : edit_state) (waypoint_name : string) (pos : pre_position) : unit =
    let pos' = { layer = s.current_layer; x = pos.x; y = pos.y } in
    assert (StaticObjectMap.get (!(get_static_object_ref s.blueprint pos')).name).traversable;
    assert (not @@ contains_waypoint_name s waypoint_name);
    remove_waypoint_at s.blueprint pos';
    s.blueprint.waypoints <- StringMap.add waypoint_name pos' s.blueprint.waypoints

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
    output_binary_string chan agent.pos.layer;
    output_binary_string chan agent.texture_name;
    output_binary_int chan (Raylib.Color.r agent.color);
    output_binary_int chan (Raylib.Color.g agent.color);
    output_binary_int chan (Raylib.Color.b agent.color);
    output_binary_int chan (Raylib.Color.a agent.color)

  let output_ambient_agent (chan : out_channel) (agent_name : string) (agent : ambient_agent_blueprint) : unit =
    output_binary_string chan agent.agent_class_name;
    output_binary_string chan agent.agent_name

  let output_region_component (chan : out_channel) (component_name : string) (component : region_component) : unit =
    output_binary_string chan component_name;
    output_binary_string chan component.layer;
    output_binary_int chan component.top;
    output_binary_int chan component.left;
    output_binary_int chan component.bottom;
    output_binary_int chan component.right

  let input_region_component (chan : in_channel) : string * region_component =
    (** Returns pair of (region component name, region component) *)
    let name = input_binary_string chan in
    let layer = input_binary_string chan in
    let top = input_binary_int chan in
    let left = input_binary_int chan in
    let bottom = input_binary_int chan in
    let right = input_binary_int chan in
    let region_component = { layer; top ; left ; bottom ; right } in
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
    let layer = input_binary_string chan in
    let texture_name = input_binary_string chan in
    let r = input_binary_int chan in
    let g = input_binary_int chan in
    let b = input_binary_int chan in
    let a = input_binary_int chan in
    {
      agent_class_name ;
      agent_name ;
      color = Raylib.Color.create r g b a;
      pos = {layer;x;y} ;
      texture_name
    }

  let input_ambient_agent (chan : in_channel) : ambient_agent_blueprint =
    let agent_class_name = input_binary_string chan in
    let agent_name = input_binary_string chan in
    { agent_class_name ; agent_name }

  let output_layer (chan : out_channel) (bp : t) (layer_name : string) (layer : layer) : unit =
    output_binary_string chan layer_name;
    output_binary_int chan layer.width;
    output_binary_int chan layer.height;
    for x = 0 to layer.width - 1 do
      for y = 0 to layer.height - 1 do
        output_static_obj chan !(get_static_object_ref bp {layer = layer_name; x; y})
      done;
    done

  let input_layer (chan : in_channel) : string * layer =
  (** Returns (layer_name, layer) read from input channel *)
    let layer_name = input_binary_string chan in
    let width = input_binary_int chan in
    let height = input_binary_int chan in
    let result_layer = create_empty_layer width height layer_name in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        let obj = input_static_obj chan in
        let { name ; color } = obj in
        set_static_object_l result_layer {x ; y} name color;
      done;
    done;
    (layer_name, result_layer)

  let serialize (bp : t) (filename : string) : unit =
    let chan = open_out_bin filename in
    output_binary_int chan (StringMap.cardinal bp.layers);
    StringMap.iter (output_layer chan bp) bp.layers;
    output_binary_int chan (StringMap.cardinal bp.agents);
    StringMap.iter (output_agent chan) bp.agents;
    output_binary_int chan (StringMap.cardinal bp.ambient_agents);
    StringMap.iter (output_ambient_agent chan) bp.ambient_agents;
    output_binary_int chan (StringMap.cardinal bp.regions);
    StringMap.iter (output_region chan) bp.regions;
    close_out chan

  let deserialize (filename : string) : t =
    let chan = open_in_bin filename in
    let result_bp = create_empty () in
    let num_layers = input_binary_int chan in
    for i = 0 to num_layers - 1 do
      let layer_name, layer = input_layer chan in
      result_bp.layers <- StringMap.add layer_name layer result_bp.layers
    done;
    let num_agents = input_binary_int chan in
    for i = 0 to num_agents - 1 do
      let agent = input_agent chan in
      add_agent_bp result_bp agent.agent_name agent.color agent.agent_class_name agent.texture_name agent.pos;
    done;
    let num_ambient_agents = input_binary_int chan in
    for i = 0 to num_ambient_agents - 1 do
      let agent = input_ambient_agent chan in
      add_ambient_agent_bp result_bp agent.agent_name agent.agent_class_name;
    done;
    let num_regions = input_binary_int chan in
    for i = 0 to num_regions - 1 do
      let region_name, region = input_region chan in
      result_bp.regions <- StringMap.add region_name region result_bp.regions;
    done;
    close_in chan;
    result_bp

  let bp_to_edit_state (bp : t) : edit_state =
    let current_layer_name =  fst (StringMap.choose bp.layers) in
    let current_layer = StringMap.find current_layer_name bp.layers in
    {
      current_layer = current_layer_name;
      blueprint = bp ;
      render_texture =
        Raylib.load_render_texture
          (current_layer.width * Config.char_width)
          (current_layer.height * Config.char_height);
    }

    let draw_prep (s : edit_state) : unit =
      let open Raylib in
      begin_texture_mode s.render_texture;
        let draw_agent (_ : string) (agent_blueprint : agent_blueprint) : unit =
          if agent_blueprint.pos.layer = s.current_layer then begin
            let { x ; y } = agent_blueprint.pos in
            let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
            let texture = TextureMap.get agent_blueprint.texture_name in
            draw_texture_ex texture pos 0.0 1.0 agent_blueprint.color
          end
        in
        let draw_waypoint (_ : string) (waypoint : position) : unit =
          if waypoint.layer = s.current_layer then begin
            let { x ; y } = waypoint in
            let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
            let texture = TextureMap.get "waypoint.png" in
            draw_texture_ex texture pos 0.0 1.0 Raylib.Color.green
          end
        in
        let draw_region_component (color : Raylib.Color.t) (_ : string) (component : region_component) : unit =
          if component.layer = s.current_layer then begin
            let width = component.right - component.left + 1 in
            let height = component.bottom - component.top + 1 in
            draw_rectangle
              (component.left * Config.char_width)
              (component.top * Config.char_height)
              (width * Config.char_width)
              (height * Config.char_height)
              color
          end
        in
        let region_color_ind = ref 0 in
        let draw_region (_ : string) (region : region) : unit =
          StringMap.iter (draw_region_component @@ Array.get region_colors !region_color_ind) region.components;
          region_color_ind := (!region_color_ind + 1) mod (Array.length region_colors)
        in
        let current_layer = get_current_layer s in
        let width = Float.of_int @@ current_layer.width * Config.char_width in
        let height = Float.of_int @@ current_layer.height * Config.char_height in
        let src = Rectangle.create 0.0 0.0 width (-. height) in
        let dest = Rectangle.create 0.0 0.0 width height in
        draw_texture_pro (RenderTexture.texture current_layer.static_bg_texture) src dest (Vector2.create 0.0 0.0) 0.0 Color.white;
        StringMap.iter draw_agent s.blueprint.agents;
        StringMap.iter draw_waypoint s.blueprint.waypoints;
        StringMap.iter draw_region s.blueprint.regions;
        draw_texture_pro (RenderTexture.texture current_layer.grid_texture) src dest (Vector2.zero ()) 0.0 Color.white;
      end_texture_mode ()

  (** [draw blueprint pos scale] *)
  let draw (s : edit_state) (pos : Raylib.Vector2.t) (scale : float) : unit =
    let open Raylib in
    clear_background Color.gray;
    let current_layer = get_current_layer s in
    let width = Float.of_int @@ current_layer.width * Config.char_width in
    let height = Float.of_int @@ current_layer.height * Config.char_height in
    let src = Rectangle.create 0.0 0.0 width (-. height) in
    let dest = Rectangle.create 0.0 0.0 (width *. scale) (height *. scale) in
    draw_texture_pro (RenderTexture.texture s.render_texture) src dest pos 0.0 Color.white;
end

type grid_cell = {
  static_object : static_object ;
  static_object_color : Raylib.Color.t ;
  agent : Agent.t option ;
}

type layer = {
  width : int ;
  (** Width of board in grid cells *)

  height : int ;
  (** Height of board in grid cells *)

  grid : grid_cell ref array array ;
  (** Full map of static objects and agents at this layer *)

  static_texture : Raylib.RenderTexture.t ;
  (** A texture depicting the full static object map *)

  render_texture : Raylib.RenderTexture.t ;
  (** Texture to render to screen *)
}

type t = {
  layers : layer StringMap.t ;
  (** Maps each layer name to a layer *)

  camera : CameraAgent.t ;

  agents : ((Agent.t * (Actions.action_result ref)) StringMap.t) ref;
  (** Maps each agent name to agent, paired with result of last action the agent yielded to board
      (or Success if the agent has not yet performed an action). *)

  mutable ambient_agents : AmbientAgent.t StringMap.t ;
  (** Maps each ambient agent name to an ambient agent *)

  mutable waypoints : position StringMap.t ;
  (** Maps each waypoint name to position *)
}

let get_cell (board : t) (pos : position) : grid_cell =
  let layer = StringMap.find pos.layer board.layers in
  !(Array.get (Array.get layer.grid pos.x) pos.y)

let set_static_object (board : t) (pos : position) (static_object : static_object) : unit =
  let layer = StringMap.find pos.layer board.layers in
  let cell = (Array.get (Array.get layer.grid pos.x) pos.y) in
  cell := { !cell with static_object }

let set_agent (board : t) (pos : position) (agent : Agent.t option) : unit =
  let layer = StringMap.find pos.layer board.layers in
  let cell = (Array.get (Array.get layer.grid pos.x) pos.y) in
  cell := { !cell with agent = agent }

let is_occupied (board : t) (pos : position) =
  ((get_cell board pos).static_object.traversable = false)
  || (Option.is_some (get_cell board pos).agent)

let add_agent (board : t) (agent : Agent.t) : unit =
  assert (not @@ is_occupied board (Agent.position agent));
  assert (not @@ StringMap.mem (Agent.name agent) !(board.agents));
  board.agents := StringMap.add (Agent.name agent) (agent, ref Actions.Success) !(board.agents)

let add_ambient_agent (board : t) (agent : AmbientAgent.t) : unit =
  assert (not @@ StringMap.mem (AmbientAgent.name agent) board.ambient_agents);
  board.ambient_agents <- StringMap.add (AmbientAgent.name agent) agent board.ambient_agents

let prep_draw_layer (board : t) (layer_name : string) (layer : layer) : unit =
  let open Raylib in
  begin_texture_mode layer.render_texture;
    let draw_agent (_ : string) ((agent,_) : Agent.t * Actions.action_result ref) : unit =
      if (Agent.position agent).layer = layer_name then begin
        let {x;y} = Agent.position agent in
        let pos = Vector2.create (Float.of_int @@ x * Config.char_width) (Float.of_int @@ y * Config.char_height) in
        draw_texture_ex (Agent.texture agent) pos 0.0 1.0 (Agent.color agent)
      end
    in
    draw_texture_ex (RenderTexture.texture layer.static_texture) (Vector2.create 0.0 0.0) 0.0 1.0 Color.white;
    StringMap.iter draw_agent !(board.agents);
  end_texture_mode ()

let prep_draw (board : t) : unit =
  StringMap.iter (prep_draw_layer board) board.layers

let draw_viewport (board : t) (vp : camera_transform) : unit =
  let open Raylib in
  let layer = StringMap.find vp.layer board.layers in
  let width = Float.of_int @@ Config.char_width * layer.width in
  let height = Float.of_int @@ Config.char_height * layer.height in
  let src = Rectangle.create 0.0 0.0 width (-. height) in
  let dest = Rectangle.create 0.0 0.0 (width *. vp.scale) (height *. vp.scale) in
  let draw_pos = vp.pos ^* vp.scale in
  draw_texture_pro (RenderTexture.texture layer.render_texture) src dest draw_pos 0.0 Color.white

let draw (board : t) : unit =
  let open Raylib in
  clear_background Color.gray;
  let viewports = CameraAgent.get_viewports board.camera in
  List.iter (draw_viewport board) viewports;
  StringMap.iter (fun _ ambient -> AmbientAgent.draw ambient) board.ambient_agents

let update (board : t) : unit =
  let open Raylib in
  let dt = Raylib.get_frame_time () in
  let update_ambient_agent (_ : string) (agent : AmbientAgent.t) : unit =
    AmbientAgent.handle_messages agent;
    AmbientAgent.update_input agent;
    AmbientAgent.resume agent
  in
  let update_agent (_ : string) ((agent, prev_result) : Agent.t * Actions.action_result ref) : unit =
    Agent.handle_messages agent;
    Agent.update_input agent;
    match Agent.resume agent !prev_result with
    | Actions.Walk (delta_x, delta_y) ->
      let pos = Agent.position agent in
      let pos' = {pos with x = pos.x + delta_x ; y = pos.y + delta_y} in
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
  StringMap.iter update_ambient_agent board.ambient_agents;
  StringMap.iter update_agent !(board.agents);
  CameraAgent.resume board.camera dt

let layer_from_blueprint (layer : Blueprint.layer) : layer =
  let open Raylib in
  let create_static_obj (static_obj_bp : Blueprint.static_object_blueprint) : grid_cell ref =
    ref {
      static_object = StaticObjectMap.get static_obj_bp.name ;
      static_object_color = static_obj_bp.color;
      agent = None
    }
  in
  let grid = Array.map (fun a -> Array.map (fun bp -> create_static_obj !bp) a) layer.static_objects in
  let width_pixels = layer.width * Config.char_width in
  let height_pixels = layer.height * Config.char_height in
  let static_texture = load_render_texture width_pixels height_pixels in
  begin_texture_mode static_texture;
    draw_rectangle 0 0 width_pixels height_pixels Color.black;
    for x = 0 to layer.width-1 do
      for y = 0 to layer.height-1 do
        let cell = Array.get (Array.get grid x) y in
        let src_rect = rect 0.0 0.0 (Float.of_int Config.char_width) (Float.of_int @@ - Config.char_height) in
        let dest_rect =
          rect
            (Float.of_int @@ x * Config.char_width)
            (Float.of_int @@ height_pixels - ((y + 1) * Config.char_height))
            (Float.of_int Config.char_width)
            (Float.of_int Config.char_height)
        in
        match (!cell).agent with
        | Some _ ->
          () (* agents are drawn per-frame *)
        | None ->
          let texture = TextureMap.get (!cell).static_object.texture_name in
          draw_texture_pro texture src_rect dest_rect (Vector2.zero ()) 1.0 (!cell).static_object_color
      done
    done;
  end_texture_mode ();
  {
    width = layer.width ;
    height = layer.height ;
    grid ;
    static_texture ;
    render_texture = load_render_texture (layer.width * Config.char_width) (layer.height * Config.char_height) ;
  }

let create_from_blueprint (blueprint : Blueprint.t) : t =
  let open Raylib in
  let open Agent in
  let layers = StringMap.map layer_from_blueprint blueprint.layers in
  let agents = ref StringMap.empty in
  let board_intf : board_interface = {
      get_waypoint = (fun (name : string) ->
        StringMap.find name blueprint.waypoints
      );
      get_region = (fun (name : string) ->
        StringMap.find name blueprint.regions
      );
      get_puppet = (fun (name : string) ->
        Agent.puppet (fst (StringMap.find name !agents))
      );
  } in
  let create_ambient_agent (agent_bp : Blueprint.ambient_agent_blueprint) : AmbientAgent.t =
    let agent_class = AmbientAgentClassMap.get agent_bp.agent_class_name in
    AmbientAgent.create board_intf agent_class
  in
  let ambient_agents = StringMap.map (fun x -> create_ambient_agent x) blueprint.ambient_agents in
  let create_agent (agent_bp : Blueprint.agent_blueprint) : Agent.t =
    let agent_class = AgentClassMap.get agent_bp.agent_class_name in
    Agent.create board_intf agent_class agent_bp.agent_name agent_bp.pos agent_bp.color
  in
  agents := StringMap.map (fun x -> (create_agent x, ref Actions.Success)) blueprint.agents;
  let add_agent (_ : string) ((agent, _) : Agent.t * Actions.action_result ref) : unit =
    let {layer = layer_name;x;y} = Agent.position agent in
    let layer = StringMap.find layer_name layers in
    let cell = Array.get (Array.get layer.grid x) y in
    cell := { !(cell) with agent = Some agent }
  in
  StringMap.iter add_agent !agents;
  {
    layers ;
    camera = CameraAgent.create board_intf (CurrentCameraClass.get ()) ;
    agents = agents;
    ambient_agents = ambient_agents ;
    waypoints = blueprint.waypoints;
  }