open Common
open Config
open Raylib
open Agent
open Edit_modes

open AmbientAgent

(* TODO: look into Raylib's built-in Camera module *)
type camera = {
  position : Raylib.Vector2.t ;
  scale : float
}

type selector_name =
  | AgentSelector
  | StaticObjectSelector
  | RegionEditor
  | AmbientAgentList
  | TextWriter
  | LineDrawer
  | LayerEditor

type editor_mode = {
  (** A selector for the current type of object we are placing in the
   level. *)

  name : selector_name ;
  (** The name of the current selector *)

  draw : Board.Blueprint.edit_state -> vec2 -> float -> vec2 -> Raylib.Rectangle.t ;
  (** [draw bp camera_pos scale mouse_pos] Draws the current selector with [bp] the currently opened blueprint,
      where [camera_pos] is the camera position of the top-left corner of the viewport relative to the
      top-left corner of the board, [scale] is the scaling factor to display the board, and [mouse_pos] is
      the view position of the mouse

      Returns the window-space rectangle that the editor GUI appears within.
  *)

  handle_keypress : Board.Blueprint.edit_state -> bool ;
  (** [handle_keypress edit_state] Handle any relevant pending keypresses, returning true if some keypress
    was handled and false otherwise *)

  handle_keypress_pos : Board.Blueprint.edit_state -> pre_position -> bool ;
  (** [handle_keypress_pos edit_state pre_position] Handle any relevant pending keypresses when the mouse cursor
    is hovering over cell position [pre_position] of the current layer, returning true if some keypress
    was handled and false otherwise *)

  mouse_click_left : Board.Blueprint.edit_state -> pre_position -> vec2 -> float -> unit ;
  (** [mouse_click_left bp cursor_cell_pos camera_pos scale] The left mouse button is clicked with the cursor over the given board position *)

  mouse_down_left : Board.Blueprint.edit_state -> pre_position -> unit ;
  (** The left mouse button is being held down over the given board position *)

  mouse_released_left : Board.Blueprint.edit_state -> position -> unit
  (** The left mouse button was released with the cursor over the given board position *)
}

(* (region_editor : t) (bp : Board.Blueprint.t) (cursor_cell_pos : position) (camera_pos : vec2) (scale : float) *)

type edit_state = {
  bp_edit_state : Board.Blueprint.edit_state ;
  mutable camera_pos : Vector2.t ;
  scale : float ref ;
  mutable edit_mode : editor_mode ;

  object_selector : ObjectSelector.t ;
  object_selector_mode : editor_mode ;

  agent_selector : AgentClassSelector.t ;
  agent_selector_mode : editor_mode ;

  region_editor : RegionEditor.t ;
  region_editor_mode : editor_mode ;

  line_drawer : LineDrawer.t ;
  line_drawer_mode : editor_mode ;

  ambient_selector_mode : editor_mode ;

  text_writer_mode : editor_mode ;

  layer_editor_mode : editor_mode ;
}

type game_state =
  (* Editing blueprint camera_pos camera_scale *)
  | Editing of edit_state
  | Playing of Board.t

let () = Printexc.record_backtrace true

(** Launches the display/update loop for a dialog to select a static object. *)
let get_static_obj () : static_object option =
  GuiTools.get_item
    StaticObjectMap.search
    (fun o -> o.name)
    ~get_texture:(fun o -> TextureMap.get o.texture_name)

let get_agent_class () : agent_class option =
  GuiTools.get_item
    AgentClassMap.search
    (fun (c : agent_class) -> c.name)
    ~get_texture:(fun (c : agent_class) ->
      TextureMap.get c.preview_texture_name)

let create_edit_state (bp_edit_state : Board.Blueprint.edit_state) : edit_state =
  let object_selector = ObjectSelector.create () in
  let agent_selector = AgentClassSelector.create () in
  let region_editor = RegionEditor.create () in
  let ambient_selector = AmbientClassSelector.create () in
  let text_writer = TextWriter.create () in
  let line_drawer = LineDrawer.create () in
  let layer_editor = LayerEditor.create () in
  (** Begin using the static object selector *)
  let object_selector_mode : editor_mode =
    let open ObjectSelector in
    {
      name = StaticObjectSelector ;
      draw = (fun _ _ _ _ -> draw object_selector) ;
      handle_keypress = (fun edit_state -> handle_keypress object_selector edit_state);
      handle_keypress_pos = (fun edit_state prepos -> handle_keypress_pos object_selector edit_state prepos);
      mouse_click_left = (fun _ _ _ _ -> ()) ;
      mouse_down_left = instantiate object_selector ;
      mouse_released_left = (fun _ _ -> ())
    }
  in

  let agent_selector_mode : editor_mode =
    let open AgentClassSelector in
    {
      name = AgentSelector ;
      draw = (fun _ _ _ _ -> draw agent_selector) ;
      handle_keypress = (fun _ -> handle_keypress agent_selector) ;
      handle_keypress_pos = (fun _ _ -> false);
      mouse_click_left = (fun edit_state cell_pos _ _ -> instantiate agent_selector edit_state cell_pos) ;
      mouse_down_left = (fun _ _ -> ()) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in

  let ambient_selector_mode : editor_mode =
    let open AmbientClassSelector in
    {
      name = AmbientAgentList ;
      draw = (fun edit_state _ _ _ -> draw ambient_selector edit_state) ;
      handle_keypress = (fun _ -> false) ;
      handle_keypress_pos = (fun _ _ -> false);
      mouse_click_left = (fun _ _ _ _ -> ()) ;
      mouse_down_left = (fun _ _ -> ()) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in
  let region_editor_mode : editor_mode =
    let open RegionEditor in
    {
      name = RegionEditor ;
      draw = (fun edit_state camera_pos scale mouse_pos -> draw region_editor edit_state camera_pos scale mouse_pos) ;
      handle_keypress = (fun _ -> false) ;
      handle_keypress_pos = (fun _ _ -> false);
      mouse_click_left = (fun edit_state cell_pos camera_pos scale-> click_left region_editor edit_state cell_pos camera_pos scale) ;
      mouse_down_left = (fun _ _ -> ()) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in
  let text_writer_mode : editor_mode =
    let open TextWriter in
    {
      name = TextWriter ;
      draw = (fun edit_state camera_pos scale mouse_pos -> draw text_writer edit_state camera_pos scale mouse_pos) ;
      handle_keypress = (fun edit_state -> handle_keypress text_writer edit_state) ;
      handle_keypress_pos = (fun _ _ -> false);
      mouse_click_left = (fun edit_state cursor_cell_pos camera_pos scale -> click_left text_writer edit_state cursor_cell_pos camera_pos scale) ;
      mouse_down_left = (fun _ _ -> ()) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in
  let line_drawer_mode : editor_mode =
    let open LineDrawer in
    {
      name = LineDrawer ;
      draw = (fun edit_state _ _ _ -> draw line_drawer edit_state) ;
      handle_keypress = (fun _ -> false) ;
      handle_keypress_pos = (fun _ _ -> false);
      mouse_click_left = (fun _ _ _ _ -> ()) ;
      mouse_down_left = (fun bp pos -> instantiate line_drawer bp pos) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in
  let layer_editor_mode : editor_mode =
    let open LayerEditor in
    {
      name = LayerEditor ;
      draw = (fun edit_state _ _ _ -> draw layer_editor edit_state) ;
      handle_keypress = (fun _ -> false) ;
      handle_keypress_pos = (fun _ _ -> false);
      mouse_click_left = (fun _ _ _ _ -> ()) ;
      mouse_down_left = (fun _ _ -> ()) ;
      mouse_released_left = (fun _ _ -> ())
    }
  in
  {
    bp_edit_state ;
    camera_pos = Vector2.create 0.0 0.0 ;
    scale = ref 1.0 ;
    edit_mode = object_selector_mode ;

    object_selector ;
    object_selector_mode ;

    agent_selector ;
    agent_selector_mode ;

    ambient_selector_mode ;

    region_editor ;
    region_editor_mode ;

    line_drawer ;
    line_drawer_mode ;

    text_writer_mode ;
    layer_editor_mode ;
  }

let () =
  Printexc.record_backtrace true;
  init_window Config.screen_width Config.screen_height "Hyperzeux";
  set_target_fps 60;

  ResourceBundleMap.add Boards.Big_board.ResourceBundle.resources;
  ResourceBundleMap.load_bundle "big_board";

  Raylib.init_audio_device ();
  let song = Raylib.load_music_stream "music/song3.mp3" in
  (* Raylib.play_music_stream song; *)

  let bp_edit_state = Board.Blueprint.create_initial_state Config.board_cells_width Config.board_cells_height "empty" in
  let game_state = ref @@ Editing (create_edit_state bp_edit_state) in
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
      let edit_state = Board.Blueprint.bp_to_edit_state bp in
      Board.Blueprint.draw_prep edit_state;
      game_state := Editing (create_edit_state edit_state)
    | None ->
      ()
  in
  while not (window_should_close () && not (is_key_pressed Key.Escape)) do
    Raylib.update_music_stream song;
    match !game_state with
    | Playing b ->
      Board.update b;
      Board.prep_draw b;
      begin_drawing ();
        Board.draw b;
      end_drawing ();
    | Editing ({ bp_edit_state ; camera_pos ; scale ; edit_mode } as edit_state) ->
      let dt = Raylib.get_frame_time () in
      let mouse_delta = Raylib.get_mouse_delta () in
      if is_mouse_button_down MouseButton.Right then
        edit_state.camera_pos <- Vector2.subtract camera_pos mouse_delta
      else
        ();

      let wheel_delta = Raylib.get_mouse_wheel_move () in
      scale := !scale +. (Config.editor_zoom_speed *. wheel_delta *. dt);

      if edit_mode.handle_keypress bp_edit_state then
        ()
      else if is_key_down Key.Left_control && is_key_pressed Key.P then
        game_state := Playing (Board.create_from_blueprint @@ Board.Blueprint.get_blueprint bp_edit_state)
      else if is_key_pressed Key.One then
        edit_state.edit_mode <- edit_state.object_selector_mode
      else if is_key_pressed Key.Two then
        edit_state.edit_mode <- edit_state.agent_selector_mode
      else if is_key_pressed Key.Three then
        edit_state.edit_mode <- edit_state.region_editor_mode
      else if is_key_pressed Key.Four then
        edit_state.edit_mode <- edit_state.ambient_selector_mode
      else if is_key_pressed Key.Five then
        edit_state.edit_mode <- edit_state.text_writer_mode
      else if is_key_pressed Key.Six then
        edit_state.edit_mode <- edit_state.line_drawer_mode
      else if is_key_pressed Key.Seven then
        edit_state.edit_mode <- edit_state.layer_editor_mode
      else if (is_key_pressed Key.O) && (is_key_down Key.Left_control) then
        begin
          let opt_obj = get_static_obj () in
          match opt_obj with
          | Some obj ->
            edit_state.edit_mode <- edit_state.object_selector_mode;
            ObjectSelector.set_obj edit_state.object_selector obj
          | None ->
            ()
        end
      else if (is_key_pressed Key.A) && (is_key_down Key.Left_control) then
        begin
          let opt_agent_class = get_agent_class () in
          match opt_agent_class with
          | Some agent_class ->
            edit_state.edit_mode <- edit_state.agent_selector_mode;
            AgentClassSelector.set_obj edit_state.agent_selector agent_class;
          | None ->
            ()
        end
      else if (is_key_pressed Key.S) && (is_key_down Key.Left_alt) then
        save_board (Board.Blueprint.get_blueprint bp_edit_state)
      else if (is_key_pressed Key.O) && (is_key_down Key.Left_alt) then
        load_board ()
      else if (is_key_pressed Key.W) then
        begin
        let {x;y} : pre_position = get_mouse_boardpos camera_pos !scale in
        if x >= 0 && y >= 0 && x < Config.board_cells_width && y < Config.board_cells_height then
          let opt_name = GuiTools.get_new_name "Enter new waypoint name" (Board.Blueprint.contains_waypoint_name bp_edit_state) in
          match opt_name with
          | Some(name) ->
              Board.Blueprint.add_waypoint bp_edit_state name {x;y};
          | None ->
            ()
        end;

      let mouse_pos = Raylib.get_mouse_position () in
      Board.Blueprint.draw_prep bp_edit_state;
        (* (region_editor : t) (bp : Board.Blueprint.t) (cursor_cell_pos : position) (camera_pos : vec2) (scale : float) *)
      begin_drawing ();
      Board.Blueprint.draw bp_edit_state camera_pos !scale;
        (* let curr_obj_texture = TextureMap.get curr_object.texture_name in *)
      let edit_rect = edit_mode.draw bp_edit_state camera_pos !scale mouse_pos in
      end_drawing ();

      if not (((Vector2.x mouse_pos) >= Rectangle.x edit_rect)
             && ((Vector2.y mouse_pos) >= Rectangle.y edit_rect)
             && ((Vector2.x mouse_pos) <= (Rectangle.x edit_rect) +. (Rectangle.width edit_rect))
             && ((Vector2.y mouse_pos) <= (Rectangle.y edit_rect) +. (Rectangle.height edit_rect))) then
        begin
          let mouse_cell_x = Int.of_float @@ ((Vector2.x mouse_pos) +. (Vector2.x camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_width)) in
          let mouse_cell_y = Int.of_float @@ ((Vector2.y mouse_pos) +. (Vector2.y camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_height)) in

          let contained_in_board (x : int) (y : int) =
            x >= 0 && y >= 0 && x < Config.board_cells_width && y < Config.board_cells_height
          in

          if Raylib.is_mouse_button_pressed MouseButton.Left && contained_in_board mouse_cell_x mouse_cell_y then
            edit_mode.mouse_click_left bp_edit_state {x = mouse_cell_x ; y = mouse_cell_y} camera_pos !scale;

          if Raylib.is_mouse_button_down MouseButton.Left && contained_in_board mouse_cell_x mouse_cell_y then
            edit_mode.mouse_down_left bp_edit_state {x = mouse_cell_x ; y = mouse_cell_y};

          ignore @@ edit_mode.handle_keypress_pos bp_edit_state {x = mouse_cell_x ; y = mouse_cell_y};
        end
  done;

  Raylib.close_window ();
