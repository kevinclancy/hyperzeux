open Common
open Config
open Raylib
open AgentClass_intf

(* TODO: look into Raylib's built-in Camera module *)
type camera = {
  position : Raylib.Vector2.t ;
  scale : float
}

(* A selector for the current type of object we are placing in the
   level. *)
type selector_state = {
  (* Draw the current selector *)
  draw : unit -> unit ;
  (* Press right arrow: select next item *)
  next : unit -> unit ;
  (* Press left arrow: select previous item *)
  prev : unit -> unit ;
  (* Instantiate the currently selected object at a given position *)
  instantiate : Board.Blueprint.t -> position -> unit
}

type edit_state = {
  blueprint : Board.Blueprint.t ;
  camera_pos : Vector2.t ref ;
  scale : float ref ;
  selector : selector_state ref ;
}

type game_state =
  (* Editing blueprint camera_pos camera_scale *)
  | Editing of edit_state
  | Playing of Board.t

let () = Printexc.record_backtrace true

(** Launches the display/update loop for a dialog to select a static object. *)
let get_static_obj () : static_object option =
  GuiTools.get_item StaticObjectMap.search (fun o -> o.name) (fun o -> TextureMap.get o.texture_name)

let get_agent_class () : (module AgentClass) option =
  GuiTools.get_item
    AgentClassMap.search
    (fun (c : (module AgentClass)) ->
      let module M = (val c : AgentClass) in M.name)
    (fun (c : (module AgentClass)) ->
      let module M = (val c : AgentClass) in TextureMap.get M.preview_texture_name)

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
  TextureMap.load "plant_1.png";
  TextureMap.load "plant_2.png";

  AgentClassMap.add (module Patroller);
  AgentClassMap.add (module Player);

  StaticObjectMap.add { name = "empty" ; texture_name = "empty_cell.png" ; traversable = true };
  StaticObjectMap.add { name = "wall" ; texture_name = "solid_wall.png" ; traversable = false };
  StaticObjectMap.add { name = "checkered_wall" ; texture_name = "checkered_wall.png" ; traversable = false };
  StaticObjectMap.add { name = "plant_1" ; texture_name = "plant_1.png" ; traversable = true };
  StaticObjectMap.add { name = "plant_2" ; texture_name = "plant_2.png" ; traversable = true };

  let object_selector = ObjectSelector.create () in

  let agent_selector = AgentClassSelector.create () in

  (** Begin using the static object selector *)
  let object_selector_state : selector_state =
    let open ObjectSelector in
    {
      draw = (fun () -> draw object_selector) ;
      next = (fun () -> next_obj object_selector) ;
      prev = (fun () -> prev_obj object_selector) ;
      instantiate = instantiate object_selector ;
    }
  in

  let agent_selector_state : selector_state =
    let open AgentClassSelector in
    {
      draw = (fun () -> draw agent_selector) ;
      next = (fun () -> next_obj agent_selector) ;
      prev = (fun () -> prev_obj agent_selector) ;
      instantiate = instantiate agent_selector
    }
  in

  let bp =
    Board.Blueprint.create_empty board_cells_width board_cells_height "empty"
  in
  let game_state =
    ref @@ Editing {
      blueprint = bp ;
      camera_pos = ref @@ Vector2.create 0.0 0.0 ;
      scale = ref 1.0 ;
      selector = ref object_selector_state
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
    | Editing { blueprint ; camera_pos ; scale ; selector } ->
      let dt = Raylib.get_frame_time () in
      let mouse_delta = Raylib.get_mouse_delta () in
      if is_mouse_button_down MouseButton.Right then
        camera_pos := Vector2.subtract !camera_pos mouse_delta
      else
        ();

      let wheel_delta = Raylib.get_mouse_wheel_move () in
      scale := !scale +. (Config.editor_zoom_speed *. wheel_delta *. dt);

      if Raylib.is_key_pressed Key.Comma then
        (!selector).prev ()
      else if is_key_pressed Key.Period then
        (!selector).next ()
      else if is_key_pressed Key.P then
        game_state := Playing (Board.create_from_blueprint blueprint)
      else if (is_key_pressed Key.O) && (is_key_down Key.Left_control) then
        begin
          let opt_obj = get_static_obj () in
          match opt_obj with
          | Some obj ->
            selector := object_selector_state;
            ObjectSelector.set_obj object_selector obj
          | None ->
            ()
        end
      else if (is_key_pressed Key.A) && (is_key_down Key.Left_control) then
        begin
          let opt_agent_class = get_agent_class () in
          match opt_agent_class with
          | Some agent_class ->
            selector := agent_selector_state;
            AgentClassSelector.set_obj agent_selector agent_class;
          | None ->
            ()
        end;

      if Raylib.is_mouse_button_pressed MouseButton.Left then
        (* TODO: compute the cell position here *)
        let mouse_pos = Raylib.get_mouse_position () in
        let x = Int.of_float @@ ((Vector2.x mouse_pos) +. (Vector2.x !camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_width)) in
        let y = Int.of_float @@ ((Vector2.y mouse_pos) +. (Vector2.y !camera_pos)) /. (!scale *. (Float.of_int @@ Config.char_height)) in
        if x >= 0 && y >= 0 && x < Config.board_cells_width && y < Config.board_cells_height then
          begin
            (!selector).instantiate blueprint {x ; y};
          end
        else
          ();
      else
        ();

      Board.Blueprint.draw_prep bp;

      begin_drawing ();
        Board.Blueprint.draw bp !camera_pos !scale;
        (* let curr_obj_texture = TextureMap.get curr_object.texture_name in *)
        (!selector).draw () ;
        (* draw_texture_ex
          curr_obj_texture
          (Vector2.create Config.(Float.of_int @@ screen_width - char_width - 50) 50.0)
          0.0
          1.0
          Color.white; *)
      end_drawing ()
  done;

  Raylib.close_window ();
