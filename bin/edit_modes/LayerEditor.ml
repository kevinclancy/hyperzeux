open Common
open LineBundleMap

let width = 350.0
let height = 680.0
let margin = 10.0

let outer_boundary_left = Config.screen_width_f -. width -. margin
let outer_boundary_top = margin

let button_width = (width -. (2. *. margin)) /. 2.
(** width of "Add Layer" and "Del Layer" buttons *)

let button_height = 28.0
(** height of "Add Layer" and "Del Layer" buttons *)

type t = {
  mutable focused_index : int ;
  mutable selected_index : int ;
  mutable scroll_index : int ;
}

let create () : t =
  {
    focused_index = -1 ;
    selected_index = -1 ;
    scroll_index = -1 ;
  }

let draw (layer_editor : t) (edit_state : Board.Blueprint.edit_state) : Raylib.Rectangle.t =
  let open Raylib in
  let boundary_left = Config.screen_width_f -. width -. margin in
  let boundary_top = margin in
  let boundary = Rectangle.create boundary_left boundary_top width height in
  draw_rectangle_rec boundary Color.black;

  (** Draw layer listbox *)
  let selected_index, focused_index, scroll_index, layer_list_bottom =
    let line_bundles_height = height -. 120.0 in
    let boundary = Rectangle.create
      (boundary_left +. margin)
      (boundary_top +. margin)
      (width -. 2. *. margin)
      line_bundles_height
    in
    let (selected,focused,scroll) = Raygui.list_view_ex
      boundary
      (Board.Blueprint.get_layer_names edit_state)
      layer_editor.focused_index
      layer_editor.scroll_index
      layer_editor.selected_index
    in
    (selected, focused, scroll, (boundary_top +. margin) +. line_bundles_height)
  in
  layer_editor.scroll_index <- scroll_index;
  layer_editor.focused_index <- focused_index;
  if selected_index <> layer_editor.selected_index then
  layer_editor.selected_index <- selected_index;

  (* draw "create", "delete", and "resize" buttons *)
  let new_pressed, del_pressed, resize_pressed, buttons_bottom =
    (* draw new region button *)
    let new_left, new_top = outer_boundary_left +. margin, layer_list_bottom +. margin in
    let new_width, new_height = ((width -. margin *. 3.) /. 2.), 30. in
    let add_boundary = Rectangle.create new_left new_top new_width new_height in
    Raygui.set_style (Default `Text_size) 18;
    let new_pressed = Raygui.button add_boundary "New" in

    let del_region_left, del_region_top = new_left +. new_width +. margin, new_top in
    let del_region_width, del_region_height = new_width, new_height in
    let del_boundary = Rectangle.create del_region_left del_region_top del_region_width del_region_height in
    Raygui.set_style (Default `Text_size) 18;
    let del_pressed = Raygui.button del_boundary "Delete" in

    let resize_left, resize_top = new_left, del_region_top +. del_region_height +. margin in
    let resize_width, resize_height = new_width, new_height in
    let resize_boundary = Rectangle.create resize_left resize_top resize_width resize_height in
    Raygui.set_style (Default `Text_size) 18;
    let resize_pressed = Raygui.button resize_boundary "Resize" in

    new_pressed, del_pressed, resize_pressed, (del_region_top +. del_region_height)
  in

  (** TODO: add modes and drawing functions for switching to a "new layer" dialog, and also a "resize layer" dialog *)

  boundary

