open Common
open AmbientAgent

let ambient_selector_width = 350.0
let ambient_selector_height = 680.0
(** Distance from agent selector to window edge *)

let ambient_listbox_height = 480.0
let ambient_selector_margin = 10.0

let button_width = (ambient_selector_width -. (2. *. ambient_selector_margin)) /. 2.
(** width of "Add Ambient" and "Del Ambient" buttons *)
let button_height = 28.0
(** height of "Add Ambient" and "Del Ambient" buttons *)

type t = {
  mutable curr_class : ambient_agent_class ref ;
  font : Raylib.Font.t ;
  mutable speed : float ;
  mutable agent_name : string ;

  mutable focused_index : int ;
  mutable selected_index : int ;
  mutable scroll_index : int ;
}

let create () : t =
  let curr_class = AmbientAgentClassMap.(get_next_elem (get_first_elem ())) in
  {
    curr_class = ref curr_class ;
    font = Raylib.load_font "fonts/romulus.png" ;
    speed = 0.5 ;
    agent_name = "joe" ;

    focused_index = -1 ;
    selected_index = -1 ;
    scroll_index = -1
  }

let draw (selector : t) (edit_state : Board.Blueprint.edit_state) =
  let open Raylib in
  let boundary_left = Config.screen_width_f -. ambient_selector_width -. ambient_selector_margin in
  let boundary_top = ambient_selector_margin in
  let boundary = Rectangle.create boundary_left boundary_top ambient_selector_width ambient_selector_height in
  draw_rectangle_rec boundary Color.black;
  let ambient_names = Board.Blueprint.ambient_names edit_state in
  let selected_index, focused_index, scroll_index, ambient_list_bottom =
    let boundary = Rectangle.create
      (boundary_left +. ambient_selector_margin)
      (boundary_top +. ambient_selector_margin)
      (ambient_selector_width -. 2. *. ambient_selector_margin)
      ambient_listbox_height
    in
    let (selected,focused,scroll) = Raygui.list_view_ex
      boundary
      ambient_names
      selector.focused_index
      selector.scroll_index
      selector.selected_index
    in
    (selected, focused, scroll, boundary_top +. ambient_listbox_height)
  in
  selector.scroll_index <- scroll_index;
  selector.focused_index <- focused_index;
  selector.selected_index <- selected_index;
  let add_pressed, add_boundary_right =
    let add_ambient_boundary =
      Rectangle.create
        (boundary_left +. ambient_selector_margin)
        (ambient_list_bottom +. ambient_selector_margin *. 2.)
        button_width
        button_height
    in
    Raygui.set_style (Default `Text_size) 18;
    Raygui.set_style (Button `Base_color_normal) 0x0f0f0fff;
    (Raygui.button add_ambient_boundary "Add Ambient"),
    (boundary_left +. ambient_selector_margin) +. button_width
  in
  if add_pressed then
    begin
      match GuiTools.get_item AmbientAgentClassMap.search (fun amb -> amb.name) with
      | Some(ambient_class) ->
        Board.Blueprint.add_ambient_agent edit_state ambient_class.name ambient_class.name
      | None ->
        ()
    end;
  let del_pressed =
    let del_boundary =
      Rectangle.create
        (add_boundary_right +. ambient_selector_margin)
        (ambient_list_bottom +. ambient_selector_margin *. 2.)
        (button_width -. ambient_selector_margin)
        button_height
    in
    Raygui.set_style (Default `Text_size) 18;
    Raygui.set_style (Button `Base_color_normal) 0x0f0f0fff;
    Raygui.button del_boundary "Del Ambient"
  in
  if del_pressed then
    begin
      ()
    end;
  boundary
