open Common

type t = {
  curr_object : static_object ref ;
  font : Raylib.Font.t ;
  color : Raylib.Color.t ref
}

let create () : t =
  {
    curr_object = ref @@ StaticObjectMap.(get_next_elem (get_first_elem ())) ;
    font = Raylib.load_font "fonts/romulus.png" ;
    color = ref Raylib.Color.white
  }

let get_curr_name (selector : t) : string =
  (!(selector.curr_object)).name

let get_curr_color (selector : t) : Raylib.Color.t =
  !(selector.color)

let set_obj (selector : t) (obj : static_object) =
  selector.curr_object := obj

let next_obj (selector : t) =
  selector.curr_object := (StaticObjectMap.get_next_elem !(selector.curr_object))

let prev_obj (selector : t) =
    selector.curr_object := (StaticObjectMap.get_prev_elem !(selector.curr_object))

let draw (selector : t) =
  let open Raylib in
  let boundary_left = Config.(screen_width_f -. object_selector_width -. object_selector_margin) in
  let boundary_top = Config.object_selector_margin in
  let boundary = Config.(Rectangle.create boundary_left boundary_top object_selector_width object_selector_height) in
  draw_rectangle_rec boundary Color.black;
  let curr_obj = !(selector.curr_object) in
  let obj_name_text = Printf.sprintf "%s" curr_obj.name in
  let obj_name_pos = Vector2.create (boundary_left +. 10.0) (boundary_top +. 10.0) in
  draw_text_ex selector.font obj_name_text obj_name_pos 32.0 1.0 Color.blue;
  let trav_text = Printf.sprintf "traversable: %b" curr_obj.traversable in
  let trav_pos = Vector2.create (boundary_left +. 10.0) (boundary_top +. 10.0 +. 42.0) in
  draw_text_ex selector.font trav_text trav_pos 32.0 1.0 Color.blue;
  let curr_obj_texture = TextureMap.get curr_obj.texture_name in
  let curr_obj_pos =
    Vector2.create
      Config.(boundary_left +. (object_selector_width /. 2.0) -. (Float.of_int @@ char_width * 2))
      (boundary_top +. 10.0 +. 84.0 +. 10.0)
  in
  draw_texture_ex curr_obj_texture curr_obj_pos 0.0 4.0 !(selector.color);
  let cntrl_text_pos =
    Vector2.create
      (boundary_left +. 14.0)
      (boundary_top +. 10.0 +. 84.0 +. 17.0) (* (boundary_top +. 10.0 +. 84.0 +. 10.0 +. (Float.of_int @@ Config.char_height * 4) +. 20.0) *)
  in
  draw_text_ex selector.font "    <   >" cntrl_text_pos 48.0 1.0 Color.blue;
  selector.color :=
    Raygui.color_picker
      (Rectangle.create (boundary_left +. 17.0) (boundary_top +. 180.0) (Config.object_selector_width -. 60.0) 80.0)
      !(selector.color)

let instantiate (selector : t) (bp : Board.Blueprint.t) (pos : position) : unit =
  let curr_object = !(selector.curr_object) in
  Board.Blueprint.set_static_object bp pos curr_object.name !(selector.color)