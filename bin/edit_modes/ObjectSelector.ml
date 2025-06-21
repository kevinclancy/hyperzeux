open Common

type t = {
  mutable curr_object : static_object ;
  font : Raylib.Font.t ;
  mutable color : Raylib.Color.t
}

let create () : t =
  {
    curr_object = StaticObjectMap.(get_next_elem (get_first_elem ())) ;
    font = Raylib.load_font "fonts/romulus.png" ;
    color = Raylib.Color.white
  }

let get_curr_name (selector : t) : string =
  selector.curr_object.name

let get_curr_color (selector : t) : Raylib.Color.t =
  selector.color

let set_obj (selector : t) (obj : static_object) =
  selector.curr_object <- obj

let next_obj (selector : t) : unit =
  selector.curr_object <- (StaticObjectMap.get_next_elem selector.curr_object)

let prev_obj (selector : t) : unit =
    selector.curr_object <- (StaticObjectMap.get_prev_elem selector.curr_object)

let handle_keypress_pos (selector : t) (edit_state : Board.Blueprint.edit_state) (cursor_cell_pos : pre_position) : bool =
  let open Raylib in
  if is_key_pressed Key.F then
    let w = Board.Blueprint.get_width edit_state in
    let h = Board.Blueprint.get_height edit_state in
    let fill_obj_name = Board.Blueprint.get_static_object_name edit_state cursor_cell_pos in
    let fill_obj_color = Board.Blueprint.get_static_object_color edit_state cursor_cell_pos in
    let q = Queue.create () in
    Queue.add cursor_cell_pos q ;
    let module PositionSet = Set.Make(
      struct
        type t = pre_position
        let equal ({x=x1;y=y1} : pre_position) ({x=x2;y=y2} : pre_position) =
          x1 == x2 && y1 == y2
        let compare ({x=x1;y=y1} : pre_position) ({x=x2;y=y2} : pre_position) =
          if x1 == x2 && y1 == y2 then 0
          else if x1 < x2 || (x1 == x2 && y1 < y2) then -1 else 1
      end
    ) in
    let visited = ref @@ PositionSet.empty in
    while not @@ Queue.is_empty q do
      let curr_pos = Queue.pop q in
      if not (PositionSet.mem curr_pos !visited)
         && curr_pos.x >= 0 && curr_pos.x < w && curr_pos.y >= 0 && curr_pos.y < h then
        begin
          visited := PositionSet.add curr_pos !visited;
          let obj_name = Board.Blueprint.get_static_object_name edit_state curr_pos in
          let obj_color = Board.Blueprint.get_static_object_color edit_state curr_pos in
          if (String.equal obj_name fill_obj_name) && (color_to_int obj_color) = (color_to_int fill_obj_color) then
            begin
              Board.Blueprint.set_static_object edit_state curr_pos selector.curr_object.name selector.color;
              for dx=(-1) to 1 do
                for dy=(-1) to 1 do
                  if not (dx == 0 && dy == 0) then
                    Queue.add {x=curr_pos.x+dx ; y=curr_pos.y+dy} q;
                done;
              done;
            end
        end
    done;
    true
  else
    false

let handle_keypress (selector : t) (edit_state : Board.Blueprint.edit_state) : bool =
  let open Raylib in
  if is_key_pressed Key.Comma then
    begin
      next_obj selector;
      true
    end
  else if is_key_pressed Key.Period then
    begin
      next_obj selector;
      true
    end
  else
    false

let draw (selector : t) =
  let open Raylib in
  let boundary_left = Config.(screen_width_f -. object_selector_width -. object_selector_margin) in
  let boundary_top = Config.object_selector_margin in
  let boundary = Config.(Rectangle.create boundary_left boundary_top object_selector_width object_selector_height) in
  draw_rectangle_rec boundary Color.black;
  let curr_obj = selector.curr_object in
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
  draw_texture_ex curr_obj_texture curr_obj_pos 0.0 4.0 selector.color;
  let cntrl_text_pos =
    Vector2.create
      (boundary_left +. 14.0)
      (boundary_top +. 10.0 +. 84.0 +. 17.0) (* (boundary_top +. 10.0 +. 84.0 +. 10.0 +. (Float.of_int @@ Config.char_height * 4) +. 20.0) *)
  in
  draw_text_ex selector.font "    <   >" cntrl_text_pos 48.0 1.0 Color.blue;
  selector.color <-
    Raygui.color_picker
      (Rectangle.create (boundary_left +. 17.0) (boundary_top +. 180.0) (Config.object_selector_width -. 60.0) 80.0)
      selector.color;
  boundary

let instantiate (selector : t) (edit_state : Board.Blueprint.edit_state) (pos : pre_position) : unit =
  Board.Blueprint.set_static_object edit_state pos selector.curr_object.name selector.color