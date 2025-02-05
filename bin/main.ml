open Common
open Config
open Raylib
open Agent
open Edit_modes

(* TODO: look into Raylib's built-in Camera module *)
type camera = {
  position : Raylib.Vector2.t ;
  scale : float
}

type selector_name =
  | AgentSelector
  | StaticObjectSelector
  | RegionEditor

type editor_mode = {
  (** A selector for the current type of object we are placing in the
   level. *)

  name : selector_name ;
  (** The name of the current selector *)

  draw : Board.Blueprint.t -> vec2 -> float -> vec2 -> unit ;
  (** [draw bp camera_pos scale mouse_pos] Draws the current selector with [bp] the currently opened blueprint,
      where [camera_pos] is the camera position of the top-left corner of the viewport relative to the
      top-left corner of the board, [scale] is the scaling factor to display the board, and [mouse_pos] is
      the view position of the mouse
  *)

  period_pressed : unit -> unit ;
  (** Period (i.e '>') pressed -- typically cycle to next placeable game object *)

  comma_pressed : unit -> unit ;
  (** Comma (i.e. '<') pressed -- typically cycle to previous placeable game object *)

  mouse_click_left : Board.Blueprint.t -> position -> vec2 -> float -> unit ;
  (** [mouse_click_left bp cursor_cell_pos camera_pos scale] The left mouse button is clicked with the cursor over the given board position *)

  mouse_down_left : Board.Blueprint.t -> position -> unit ;
  (** The left mouse button is being held down over the given board position *)

  mouse_released_left : Board.Blueprint.t -> position -> unit
  (** The left mouse button was released with the cursor over the given board position *)
}

(* (region_editor : t) (bp : Board.Blueprint.t) (cursor_cell_pos : position) (camera_pos : vec2) (scale : float) *)

type edit_state = {
  blueprint : Board.Blueprint.t ;
  camera_pos : Vector2.t ref ;
  scale : float ref ;
  edit_mode : editor_mode ref ;
}

type game_state =
  (* Editing blueprint camera_pos camera_scale *)
  | Editing of edit_state
  | Playing of Board.t

let () = Printexc.record_backtrace true

(** Launches the display/update loop for a dialog to select a static object. *)
let get_static_obj () : static_object option =
  GuiTools.get_item StaticObjectMap.search (fun o -> o.name) (fun o -> TextureMap.get o.texture_name)

let get_agent_class () : agent_class option =
  GuiTools.get_item
    AgentClassMap.search
    (fun (c : agent_class) -> c.name)
    (fun (c : agent_class) ->
      TextureMap.get c.preview_texture_name)

let () =
  Printexc.record_backtrace true;
  init_window Config.screen_width Config.screen_height "Visions of Evermore";
  set_target_fps 60;

  (* let texture_map : Raylib.Texture.t StringMap.t ref = ref StringMap.empty in
  texture_map := load !texture_map "person_forward_recon.png"; *)
  TextureMap.load "person_south_recon.png";
  TextureMap.load "person_north_recon.png";
  TextureMap.load "person_east_recon.png";
  TextureMap.load "person_west_recon.png";
  TextureMap.load "person2_south.png";
  TextureMap.load "person2_north.png";
  TextureMap.load "person2_east.png";
  TextureMap.load "person2_west.png";
  TextureMap.load "empty_cell.png";
  TextureMap.load "solid_wall.png";
  TextureMap.load "checkered_wall.png";
  TextureMap.load "plant_1.png";
  TextureMap.load "plant_2.png";
  TextureMap.load "waypoint.png";

  AgentClassMap.add Agents.Button.button_class;
  AgentClassMap.add Agents.Patroller.patroller_class;
  AgentClassMap.add Agents.Player.player_class;

  StaticObjectMap.add { name = "empty" ; texture_name = "empty_cell.png" ; traversable = true };
  StaticObjectMap.add { name = "wall" ; texture_name = "solid_wall.png" ; traversable = false };
  StaticObjectMap.add { name = "checkered_wall" ; texture_name = "checkered_wall.png" ; traversable = false };
  StaticObjectMap.add { name = "plant_1" ; texture_name = "plant_1.png" ; traversable = true };
  StaticObjectMap.add { name = "plant_2" ; texture_name = "plant_2.png" ; traversable = true };

  let object_selector = ObjectSelector.create () in

  let agent_selector = AgentClassSelector.create () in

  let region_editor = RegionEditor.create () in

  (** Begin using the static object selector *)
  let object_selector_state : editor_mode =
    let open ObjectSelector in
    {
      name = StaticObjectSelector ;
      draw = (fun _ _ _ _ -> draw object_selector) ;
      period_pressed = (fun () -> next_obj object_selector) ;
      comma_pressed = (fun () -> prev_obj object_selector) ;
      mouse_click_left = (fun _ _ _ _ -> ()) ;
      mouse_down_left = instantiate object_selector ;
      mouse_released_left = (fun _ _ -> ())
    }
  in

  let agent_selector_state : editor_mode =
    let open AgentClassSelector in
    {
      name = AgentSelector ;
      draw = (fun _ _ _ _ -> draw agent_selector) ;
      period_pressed = (fun () -> next_obj agent_selector) ;
      comma_pressed = (fun () -> prev_obj agent_selector) ;
      mouse_click_left = (fun bp cell_pos _ _ -> instantiate agent_selector bp cell_pos) ;
      mouse_down_left = (fun _ _ -> ()) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in

  let region_editor_mode : editor_mode =
    let open RegionEditor in
    {
      name = RegionEditor ;
      draw = (fun bp camera_pos scale mouse_pos -> draw region_editor bp camera_pos scale mouse_pos) ;
      period_pressed = (fun () -> ()) ;
      comma_pressed = (fun () -> ()) ;
      mouse_click_left = (fun bp cell_pos camera_pos scale-> click_left region_editor bp cell_pos camera_pos scale) ;
      mouse_down_left = (fun _ _ -> ()) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in
  let game_state =
    ref @@ Editing {
      blueprint = Board.Blueprint.create_empty board_cells_width board_cells_height "empty" ;
      camera_pos = ref @@ Vector2.create 0.0 0.0 ;
      scale = ref 1.0 ;
      edit_mode = ref object_selector_state
    }
  in
  let save_board (bp : Board.Blueprint.t) : unit =
    let opt_filename = GuiTools.get_input_string "Enter filename to save board to" in
    match opt_filename with
    | Some(filename) ->
      Board.Blueprint.serialize bp (String.concat "/" [Filename.current_dir_name ; filename])
    | None ->
      ()
  in
  let load_board () : unit =
    let opt_filename = GuiTools.get_input_string "Enter filename to load board from" in
    match opt_filename with
    | Some(filename) ->
      let bp = Board.Blueprint.deserialize filename in
      game_state := Editing {
        blueprint = bp ;
        camera_pos = ref @@ Vector2.create 0.0 0.0 ;
        scale = ref 1.0 ;
        edit_mode = ref object_selector_state
      }
    | None ->
      ()
  in
  while not (window_should_close ()) do
    match !game_state with
    | Playing b ->
      Board.update b;
      Board.prep_draw b;
      begin_drawing ();
        Board.draw b (Vector2.zero ()) 4.0;
      end_drawing ();
    | Editing { blueprint ; camera_pos ; scale ; edit_mode } ->
      let dt = Raylib.get_frame_time () in
      let mouse_delta = Raylib.get_mouse_delta () in
      if is_mouse_button_down MouseButton.Right then
        camera_pos := Vector2.subtract !camera_pos mouse_delta
      else
        ();

      let wheel_delta = Raylib.get_mouse_wheel_move () in
      scale := !scale +. (Config.editor_zoom_speed *. wheel_delta *. dt);

      if Raylib.is_key_pressed Key.Comma then
        (!edit_mode).comma_pressed ()
      else if is_key_pressed Key.Period then
        (!edit_mode).period_pressed ()
      else if is_key_down Key.Left_control && is_key_pressed Key.P then
        game_state := Playing (Board.create_from_blueprint blueprint)
      else if is_key_pressed Key.One then
        edit_mode := object_selector_state
      else if is_key_pressed Key.Two then
        edit_mode := agent_selector_state
      else if is_key_pressed Key.Three then
        edit_mode := region_editor_mode
      else if (is_key_pressed Key.O) && (is_key_down Key.Left_control) then
        begin
          let opt_obj = get_static_obj () in
          match opt_obj with
          | Some obj ->
            edit_mode := object_selector_state;
            ObjectSelector.set_obj object_selector obj
          | None ->
            ()
        end
      else if (is_key_pressed Key.A) && (is_key_down Key.Left_control) then
        begin
          let opt_agent_class = get_agent_class () in
          match opt_agent_class with
          | Some agent_class ->
            edit_mode := agent_selector_state;
            AgentClassSelector.set_obj agent_selector agent_class;
          | None ->
            ()
        end
      else if (is_key_pressed Key.S) && (is_key_down Key.Left_alt) then
        save_board blueprint
      else if (is_key_pressed Key.O) && (is_key_down Key.Left_alt) then
        load_board ()
      else if (is_key_pressed Key.W) then
        begin
        let {x;y} = get_mouse_boardpos !camera_pos !scale in
        if x >= 0 && y >= 0 && x < Config.board_cells_width && y < Config.board_cells_height then
          let opt_name = GuiTools.get_new_name "Enter new waypoint name" (Board.Blueprint.contains_waypoint_name blueprint) in
          match opt_name with
          | Some(name) ->
              Board.Blueprint.add_waypoint blueprint name {x;y};
          | None ->
            ()
        end;

      let mouse_pos = Raylib.get_mouse_position () in
      let mouse_cell_x = Int.of_float @@ ((Vector2.x mouse_pos) +. (Vector2.x !camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_width)) in
      let mouse_cell_y = Int.of_float @@ ((Vector2.y mouse_pos) +. (Vector2.y !camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_height)) in

      let contained_in_board (x : int) (y : int) =
        x >= 0 && y >= 0 && x < Config.board_cells_width && y < Config.board_cells_height
      in

      if Raylib.is_mouse_button_pressed MouseButton.Left && contained_in_board mouse_cell_x mouse_cell_y then
          (!edit_mode).mouse_click_left blueprint {x = mouse_cell_x ; y = mouse_cell_y} !camera_pos !scale;

      if Raylib.is_mouse_button_down MouseButton.Left && contained_in_board mouse_cell_x mouse_cell_y then
        (!edit_mode).mouse_down_left blueprint {x = mouse_cell_x ; y = mouse_cell_y};

      Board.Blueprint.draw_prep blueprint;
        (* (region_editor : t) (bp : Board.Blueprint.t) (cursor_cell_pos : position) (camera_pos : vec2) (scale : float) *)
      begin_drawing ();
        Board.Blueprint.draw blueprint !camera_pos !scale;
        (* let curr_obj_texture = TextureMap.get curr_object.texture_name in *)
        (!edit_mode).draw blueprint !camera_pos !scale mouse_pos;
        (* draw_texture_ex
          curr_obj_texture
          (Vector2.create Config.(Float.of_int @@ screen_width - char_width - 50) 50.0)
          0.0
          1.0
          Color.white; *)
      end_drawing ()
  done;

  Raylib.close_window ();
