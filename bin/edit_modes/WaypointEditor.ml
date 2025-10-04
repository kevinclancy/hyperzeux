open Common
open LineBundleMap

let width = 350.0
let height = 680.0
let margin = 10.0

let outer_boundary_left = Config.screen_width_f -. width -. margin
let outer_boundary_top = margin

let button_width = (width -. (2. *. margin)) /. 2.
(** width of "Add Waypoint" and "Del Waypoint" buttons *)

let button_height = 28.0
(** height of "Add Waypoint" and "Del Waypoint" buttons *)

let description_height = 200.0
(** height of the description text box *)

type t = {
  mutable focused_index : int ;
  mutable selected_index : int ;
  mutable scroll_index : int ;
  mutable description_text : string ;
}

let create () : t =
  {
    focused_index = -1 ;
    selected_index = -1 ;
    scroll_index = -1 ;
    description_text = "" ;
  }

let get_waypoint_names (edit_state : Board.Blueprint.edit_state) : string list =
  let waypoints = Board.Blueprint.get_waypoints edit_state in
  List.map fst (StringMap.to_list waypoints)

let draw (waypoint_editor : t) (edit_state : Board.Blueprint.edit_state) : Raylib.Rectangle.t =
  let open Raylib in
  let boundary_left = Config.screen_width_f -. width -. margin in
  let boundary_top = margin in
  let boundary = Rectangle.create boundary_left boundary_top width height in
  draw_rectangle_rec boundary Color.black;

  let waypoint_names = get_waypoint_names edit_state in

  (** Draw waypoint listbox *)
  let selected_index, focused_index, scroll_index, waypoint_list_bottom =
    let list_height = height -. description_height -. 180.0 in
    let boundary = Rectangle.create
      (boundary_left +. margin)
      (boundary_top +. margin)
      (width -. 2. *. margin)
      list_height
    in
    let (selected,focused,scroll) = Raygui.list_view_ex
      boundary
      waypoint_names
      waypoint_editor.focused_index
      waypoint_editor.scroll_index
      waypoint_editor.selected_index
    in
    (selected, focused, scroll, (boundary_top +. margin) +. list_height)
  in
  waypoint_editor.scroll_index <- scroll_index;
  waypoint_editor.focused_index <- focused_index;

  (* Update selected waypoint and load its description *)
  if selected_index <> waypoint_editor.selected_index then begin
    waypoint_editor.selected_index <- selected_index;
    if selected_index <> -1 && selected_index < List.length waypoint_names then begin
      let waypoint_name = List.nth waypoint_names selected_index in
      let waypoints = Board.Blueprint.get_waypoints edit_state in
      let waypoint = StringMap.find waypoint_name waypoints in
      waypoint_editor.description_text <- waypoint.description
    end else begin
      waypoint_editor.description_text <- ""
    end
  end;

  (* Draw description label *)
  let desc_label_top = waypoint_list_bottom +. margin in
  Raygui.set_style (Default `Text_size) 16;
  Raygui.label
    (Rectangle.create (boundary_left +. margin) desc_label_top (width -. 2. *. margin) 20.)
    "Description:";

  (* Draw description text box *)
  let desc_box_top = desc_label_top +. 25. in
  let desc_box = Rectangle.create
    (boundary_left +. margin)
    desc_box_top
    (width -. 2. *. margin)
    description_height
  in
  Raygui.set_style (Default `Text_size) 14;
  let (new_text, edit_mode) = Raygui.text_box_multi
    desc_box
    waypoint_editor.description_text
    (waypoint_editor.selected_index <> -1)
  in
  waypoint_editor.description_text <- new_text;

  (* Update the waypoint's description if it changed and we have a selection *)
  if waypoint_editor.selected_index <> -1 && waypoint_editor.selected_index < List.length waypoint_names then begin
    let waypoint_name = List.nth waypoint_names waypoint_editor.selected_index in
    let waypoints = Board.Blueprint.get_waypoints edit_state in
    let waypoint = StringMap.find waypoint_name waypoints in
    if waypoint.description <> waypoint_editor.description_text then begin
      let updated_waypoints = StringMap.add
        waypoint_name
        { waypoint with description = waypoint_editor.description_text }
        waypoints
      in
      Board.Blueprint.set_waypoints edit_state updated_waypoints
    end
  end;

  (* draw "delete" button *)
  let buttons_top = desc_box_top +. description_height +. margin in
  let del_pressed, buttons_bottom =
    let del_left, del_top = outer_boundary_left +. margin, buttons_top in
    let del_width, del_height = width -. 2. *. margin, 30. in
    let del_boundary = Rectangle.create del_left del_top del_width del_height in
    Raygui.set_style (Default `Text_size) 18;
    let del_pressed = Raygui.button del_boundary "Delete" in
    del_pressed, (del_top +. del_height)
  in

  if del_pressed && waypoint_editor.selected_index <> -1 && waypoint_editor.selected_index < List.length waypoint_names then begin
    let waypoint_name = List.nth waypoint_names waypoint_editor.selected_index in
    let waypoints = Board.Blueprint.get_waypoints edit_state in
    let updated_waypoints = StringMap.remove waypoint_name waypoints in
    Board.Blueprint.set_waypoints edit_state updated_waypoints;
    waypoint_editor.selected_index <- -1;
    waypoint_editor.description_text <- ""
  end;

  boundary

let instantiate (waypoint_editor : t) (edit_state : Board.Blueprint.edit_state) (pos : pre_position) : unit =
  (** Called when left-clicking in waypoint mode to place a waypoint *)
  let opt_name = GuiTools.get_new_name "Enter new waypoint name" (Board.Blueprint.contains_waypoint_name edit_state) in
  match opt_name with
  | Some(name) ->
    Board.Blueprint.add_waypoint edit_state name pos "";
    waypoint_editor.selected_index <- -1;
    waypoint_editor.description_text <- ""
  | None ->
    ()

let select_waypoint (waypoint_editor : t) (edit_state : Board.Blueprint.edit_state) (waypoint_name : string) : unit =
  (** Selects the waypoint with the given name in the editor *)
  let waypoint_names = get_waypoint_names edit_state in
  let rec find_index lst idx =
    match lst with
    | [] -> -1
    | h :: t -> if h = waypoint_name then idx else find_index t (idx + 1)
  in
  let index = find_index waypoint_names 0 in
  if index <> -1 then begin
    waypoint_editor.selected_index <- index;
    let waypoints = Board.Blueprint.get_waypoints edit_state in
    let waypoint = StringMap.find waypoint_name waypoints in
    waypoint_editor.description_text <- waypoint.description
  end
