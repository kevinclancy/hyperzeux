open Common
open Config
open Raylib

(* TODO: look into Raylib's built-in Camera module *)
type camera = {
  position : Raylib.Vector2.t ;
  scale : float
}

type game_state =
  (* Editing blueprint camera_pos camera_scale *)
  | Editing of Board.Blueprint.t * (Raylib.Vector2.t ref) * (float ref)
  | Playing of Board.t

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

  AgentClassMap.add "patroller" (module Patroller);

  StaticObjectMap.add "empty" { texture_name = "empty_cell.png" ; traversable = true };
  StaticObjectMap.add "wall" { texture_name = "solid_wall.png" ; traversable = true };
  (* let agent = Patroller.create "bob" { x = 2 ; y = 2 } in
  let empty_obj = { texture_name = "empty_cell.png" ; traversable = true } in *)
  (* let static_obj_map = IntMap.of_list [(0, { texture_name = "empty_cell.png" ; traversable = true })] in *)
  (* let board = Board.create_empty board_cells_width board_cells_height empty_obj in *)
  let bp =
    Board.Blueprint.create_empty board_cells_width board_cells_height "empty"
  in
  Board.Blueprint.set_static_object_key bp {x = 1 ; y = 1} "wall";
  Board.Blueprint.add_agent bp "marvin" "patroller" "person_south_recon.png" {x = 3 ; y = 3};
  let game_state = Editing (bp, ref @@ Vector2.create 0.0 0.0, ref 1.0) in
  while not (window_should_close ()) do
    match game_state with
    | Playing b ->
      failwith "todo"
    | Editing (bp, pos, scale) ->
      let dt = Raylib.get_frame_time () in
      let mouse_delta = Raylib.get_mouse_delta () in
      if is_mouse_button_down MouseButton.Right then
        pos := Vector2.subtract !pos mouse_delta
      else
        ();

      let wheel_delta = Raylib.get_mouse_wheel_move () in
      scale := !scale +. (Config.editor_zoom_speed *. wheel_delta *. dt);

      Board.Blueprint.draw bp !pos !scale;
  done;

  Raylib.close_window ();
