open Common
open LineBundleMap

let list_drawer_width = 350.0
let list_drawer_height = 680.0

let bundle_listbox_height = 480.0
let list_drawer_margin = 10.0

let button_width = (list_drawer_width -. (2. *. list_drawer_margin)) /. 2.
(** width of "Add Ambient" and "Del Ambient" buttons *)
let button_height = 28.0
(** height of "Add Ambient" and "Del Ambient" buttons *)

type t = {
  mutable curr_bundle : line_drawing_bundle ;
  mutable bundle_piece_names : Set.Make(String).t ;
  mutable focused_index : int ;
  mutable selected_index : int ;
  mutable scroll_index : int ;
  mutable color : Raylib.Color.t ;
  line_type_names : string list
}

let create () : t =
  let curr_bundle = LineBundleMap.(get_next_elem (get_first_elem ())) in
  {
    curr_bundle ;
    bundle_piece_names = LineBundleMap.all_piece_names curr_bundle ;
    focused_index = -1 ;
    selected_index = -1 ;
    scroll_index = -1 ;
    color = Raylib.Color.white ;
    line_type_names = LineBundleMap.get_all_names ()
  }

let select_index (line_drawer : t) (ind : int) : unit =
  let selected_bundle = LineBundleMap.get (List.nth line_drawer.line_type_names ind) in
  line_drawer.curr_bundle <- selected_bundle

let draw (line_drawer : t) (bp : Board.Blueprint.t) : Raylib.Rectangle.t =
  let open Raylib in
  let boundary_left = Config.screen_width_f -. list_drawer_width -. list_drawer_margin in
  let boundary_top = list_drawer_margin in
  let boundary = Rectangle.create boundary_left boundary_top list_drawer_width list_drawer_height in
  draw_rectangle_rec boundary Color.black;


  let selected_index, focused_index, scroll_index, line_bundles_bottom =
    let line_bundles_height = list_drawer_height -. 120.0 in
    let boundary = Rectangle.create
      (boundary_left +. list_drawer_margin)
      (boundary_top +. list_drawer_margin)
      (list_drawer_width -. 2. *. list_drawer_margin)
      line_bundles_height
    in
    let (selected,focused,scroll) = Raygui.list_view_ex
      boundary
      line_drawer.line_type_names
      line_drawer.focused_index
      line_drawer.scroll_index
      line_drawer.selected_index
    in
    (selected, focused, scroll, (boundary_top +. list_drawer_margin) +. line_bundles_height)
  in
  line_drawer.scroll_index <- scroll_index;
  line_drawer.focused_index <- focused_index;
  if selected_index <> line_drawer.selected_index then

  line_drawer.selected_index <- selected_index;

  let line_drawer_boundary =
    Rectangle.create
      (boundary_left +. 17.0)
      (line_bundles_bottom +. list_drawer_margin)
      200.0
      80.0
  in
  line_drawer.color <- Raygui.color_picker line_drawer_boundary line_drawer.color ;
  boundary

let update_line_at (line_drawer : t) (bp : Board.Blueprint.t) (pos : position) : unit =
  let is_line_at (pos : position) : bool =
    if Board.Blueprint.contains_pos bp pos then
      StringSet.mem (Board.Blueprint.get_static_object_name bp pos) line_drawer.bundle_piece_names
    else
      false
  in
  let is_line_n = is_line_at { pos with y = pos.y - 1 } in
  let is_line_s = is_line_at { pos with y = pos.y + 1 } in
  let is_line_w = is_line_at { pos with x = pos.x - 1 } in
  let is_line_e = is_line_at { pos with x = pos.x + 1 } in

  let str_n = if is_line_n then "n" else "x" in
  let str_e = if is_line_e then "e" else "x" in
  let str_s = if is_line_s then "s" else "x" in
  let str_w = if is_line_w then "w" else "x" in

  let line_type = Printf.sprintf "%s%s%s%s" str_n str_e str_s str_w in
  let line_type =
    if String.equal line_type "xxxw" then
      "xexw"
    else if String.equal line_type "xexx" then
      "xexw"
    else if String.equal line_type "nxxx" then
      "nxsx"
    else if String.equal line_type "xxsx" then
      "nxsx"
    else
      line_type
  in
  let obj_name =
    Printf.sprintf "%s_%s" line_drawer.curr_bundle.name line_type
  in
  Board.Blueprint.set_static_object bp pos obj_name line_drawer.color


let instantiate (line_drawer : t) (bp : Board.Blueprint.t) (pos : position) : unit =
  let is_line_at (pos : position) : bool =
    if Board.Blueprint.contains_pos bp pos then
      StringSet.mem (Board.Blueprint.get_static_object_name bp pos) line_drawer.bundle_piece_names
    else
      false
  in
  update_line_at line_drawer bp pos;
  if is_line_at { pos with y = pos.y - 1 } then
    update_line_at line_drawer bp { pos with y = pos.y - 1 };
  if is_line_at { pos with y = pos.y + 1 } then
    update_line_at line_drawer bp { pos with y = pos.y + 1 };
  if is_line_at { pos with x = pos.x - 1 } then
    update_line_at line_drawer bp { pos with x = pos.x - 1 };
  if is_line_at { pos with x = pos.x + 1 } then
    update_line_at line_drawer bp { pos with x = pos.x + 1 };

    (*
  let is_line_n = is_line_at { cursor_cell_pos with y = cursor_cell_pos.y - 1 } in
  let is_line_s = is_line_at { cursor_cell_pos with y = cursor_cell_pos.y + 1 } in
  let is_line_w = is_line_at { cursor_cell_pos with x = cursor_cell_pos.x - 1 } in
  let is_line_e = is_line_at { cursor_cell_pos with x = cursor_cell_pos.x + 1 } in

  let str_n = if is_line_n then "n" else "x" in
  let str_e = if is_line_e then "e" else "x" in
  let str_s = if is_line_s then "s" else "x" in
  let str_w = if is_line_w then "w" else "x" in

  let line_type = Printf.sprintf "%s%s%s%s" str_n str_e str_s str_w in
  let line_type =
    if String.equal line_type "xxxw" then
      "xexw"
    else if String.equal line_type "xexx" then
      "xexw"
    else if String.equal line_type "nxxx" then
      "nxsx"
    else if String.equal line_type "xxsx" then
      "nxsx"
    else
      line_type
  in
  let obj_name =
    Printf.sprintf "%s_%s" line_drawer.curr_bundle.name line_type
  in
  Printf.printf "%s\n" obj_name;
  Board.Blueprint.set_static_object bp cursor_cell_pos obj_name line_drawer.color *)

