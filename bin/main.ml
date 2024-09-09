open Common
open Config
open Raylib

(* TODO: look into Raylib's built-in Camera module *)
type camera = {
  position : Raylib.Vector2.t ;
  scale : float
}

type edit_state = {
  blueprint : Board.Blueprint.t ;
  camera_pos : Vector2.t ref ;
  scale : float ref ;
  object_selector : ObjectSelector.t ;
}

type game_state =
  (* Editing blueprint camera_pos camera_scale *)
  | Editing of edit_state
  | Playing of Board.t

let draw_object_selector (selected_obj : static_object) : unit =
  let boundary_left = Config.(screen_width_f -. object_selector_width -. object_selector_margin) in
  let boundary_top = Config.object_selector_margin in
  let boundary = Config.(Rectangle.create boundary_left boundary_top object_selector_width object_selector_height) in
  draw_rectangle_rec boundary Color.black
  (* draw_text
  draw_text_ex (Int.of_float @@ boundary_left +. 10.0) (Int.of_float @@ boundary_top +. 10.0) "Selected Object:" Color.white *)

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
  TextureMap.load "empty_cell.png";
  TextureMap.load "solid_wall.png";
  TextureMap.load "checkered_wall.png";

  AgentClassMap.add "patroller" (module Patroller);

  StaticObjectMap.add { name = "empty" ; texture_name = "empty_cell.png" ; traversable = true };
  StaticObjectMap.add { name = "wall" ; texture_name = "solid_wall.png" ; traversable = false };
  StaticObjectMap.add { name = "checkered_wall" ; texture_name = "checkered_wall.png" ; traversable = false };

  let bp =
    Board.Blueprint.create_empty board_cells_width board_cells_height "empty"
  in
  Board.Blueprint.set_static_object bp {x = 1 ; y = 1} "wall" Color.gold;
  Board.Blueprint.add_agent bp "marvin" "patroller" "person_south_recon.png" {x = 3 ; y = 3};
  let game_state =
    ref @@ Editing {
      blueprint = bp;
      camera_pos = ref @@ Vector2.create 0.0 0.0 ;
      scale = ref 1.0 ;
      object_selector = ObjectSelector.create ()
    }
  in
  while not (window_should_close ()) do
    match !game_state with
    | Playing b ->
      Board.update b;
      Board.prep_draw b;
      begin_drawing ();
        Board.draw b (Vector2.zero ()) 4.0;
      end_drawing ();
    | Editing { blueprint ; camera_pos ; scale ; object_selector } ->
      let dt = Raylib.get_frame_time () in
      let mouse_delta = Raylib.get_mouse_delta () in
      if is_mouse_button_down MouseButton.Right then
        camera_pos := Vector2.subtract !camera_pos mouse_delta
      else
        ();

      let wheel_delta = Raylib.get_mouse_wheel_move () in
      scale := !scale +. (Config.editor_zoom_speed *. wheel_delta *. dt);

      if Raylib.is_key_pressed Key.Comma then
        ObjectSelector.prev_obj object_selector
      else if is_key_pressed Key.Period then
        ObjectSelector.next_obj object_selector
      else if is_key_pressed Key.P then
        game_state := Playing (Board.create_from_blueprint blueprint);

      if Raylib.is_mouse_button_down MouseButton.Left then
        (* TODO: compute the cell position here *)
        let mouse_pos = Raylib.get_mouse_position () in
        let x = Int.of_float @@ ((Vector2.x mouse_pos) +. (Vector2.x !camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_width)) in
        let y = Int.of_float @@ ((Vector2.y mouse_pos) +. (Vector2.y !camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_height)) in
        if x >= 0 && y >= 0 && x < Config.board_cells_width && y < Config.board_cells_height then
          let name = ObjectSelector.get_curr_name object_selector in
          let color = ObjectSelector.get_curr_color object_selector in
          Board.Blueprint.set_static_object blueprint {x = x; y = y} name color
        else
          ();
      else
        ();

      Board.Blueprint.draw_prep bp;

      begin_drawing ();
        Board.Blueprint.draw bp !camera_pos !scale;
        (* let curr_obj_texture = TextureMap.get curr_object.texture_name in *)
        ObjectSelector.draw object_selector ;
        (* draw_texture_ex
          curr_obj_texture
          (Vector2.create Config.(Float.of_int @@ screen_width - char_width - 50) 50.0)
          0.0
          1.0
          Color.white; *)
      end_drawing ()
  done;

  Raylib.close_window ();
