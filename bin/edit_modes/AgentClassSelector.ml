open Common
open AgentClass_intf

type t = {
  curr_class : (module AgentClass) ref ;
  font : Raylib.Font.t ;
  color : Raylib.Color.t ref ;
  speed : float ref ;
  agent_name : string ref
}

let create () : t =
  let curr_class = AgentClassMap.(get_next_elem (get_first_elem ())) in
  let module CurrentClass = (val curr_class : AgentClass) in
  {
    curr_class = ref @@ curr_class ;
    font = Raylib.load_font "fonts/romulus.png" ;
    color = ref CurrentClass.preview_color ;
    speed = ref 0.5 ;
    agent_name = ref "joe"
  }

let get_curr_name (selector : t) : string =
  let module M = (val !(selector.curr_class) : AgentClass) in
  M.name

let update_gui (selector : t) : unit =
  let module AgentClass = (val !(selector.curr_class) : AgentClass) in
  selector.color := AgentClass.preview_color;
  selector.speed := 0.5

let get_curr_color (selector : t) : Raylib.Color.t =
  !(selector.color)

let set_obj (selector : t) (agent_class : (module AgentClass)) =
  selector.curr_class := agent_class;
  update_gui selector

let next_obj (selector : t) =
  selector.curr_class := (AgentClassMap.get_next_elem !(selector.curr_class));
  update_gui selector

let prev_obj (selector : t) =
    selector.curr_class := (AgentClassMap.get_next_elem !(selector.curr_class));
    update_gui selector

let draw (selector : t) =
  let open Raylib in
  let boundary_left = Config.(screen_width_f -. agent_selector_width -. agent_selector_margin) in
  let boundary_top = Config.object_selector_margin in
  let boundary = Config.(Rectangle.create boundary_left boundary_top agent_selector_width agent_selector_height) in
  draw_rectangle_rec boundary Color.black;
  let module CurrentAgentClass = (val !(selector.curr_class) : AgentClass) in
  let obj_name_text = Printf.sprintf "%s" CurrentAgentClass.name in
  let obj_name_pos = Vector2.create (boundary_left +. 10.0) (boundary_top +. 10.0) in
  draw_text_ex selector.font obj_name_text obj_name_pos 32.0 1.0 Color.blue;
  let slider_rect =
    Rectangle.create (boundary_left +. 50.0) (boundary_top +. 52.0) (Config.agent_selector_width -. 100.0) 20.0
  in
  selector.speed := Raygui.slider slider_rect "slow" "fast" !(selector.speed) ~min:0.0 ~max:1.0;
  let curr_class_texture = TextureMap.get CurrentAgentClass.preview_texture_name in
  let curr_class_pos =
    Vector2.create
      Config.(boundary_left +. (agent_selector_width /. 2.0) -. (Float.of_int @@ char_width * 2))
      (boundary_top +. 10.0 +. 84.0 +. 10.0)
  in
  draw_texture_ex curr_class_texture curr_class_pos 0.0 4.0 !(selector.color);
  let cntrl_text_pos =
    Vector2.create
      (boundary_left +. 37.0)
      (boundary_top +. 10.0 +. 84.0 +. 17.0)
  in
  draw_text_ex selector.font "    <   >" cntrl_text_pos 48.0 1.0 Color.blue;
  selector.color :=
    Raygui.color_picker
      (Rectangle.create (boundary_left +. 17.0) (boundary_top +. 180.0) (Config.agent_selector_width -. 60.0) 80.0)
      !(selector.color)

let instantiate (selector : t) (bp : Board.Blueprint.t) (pos : position) : unit =
  let module M = (val !(selector.curr_class) : AgentClass) in
  match GuiTools.get_new_name "Enter new agent name" (Board.Blueprint.contains_agent_name bp) with
  | Some(name) ->
    Board.Blueprint.add_agent bp name !(selector.color) M.name M.preview_texture_name pos
  | None ->
    ()