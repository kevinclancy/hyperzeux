open Common
open Region

type menu_state = {
  mutable region_list_scroll_index : int ;
  (** The region list's scrollbar position *)

  mutable focused_region_index : int ;
  (** index of region name that mouse is hovering over (or -1 if not over an item) *)

  mutable selected_region_index : int ;
  (** The index of the currently selected region *)

  mutable square_list_scroll_index : int ;
  (** The square list's scrollbar position *)

  mutable focused_square_index : int ;
  (** index of square name that mouse is hovering over (or -1 if not over an item) *)

  mutable selected_square_index : int ;
  (** The index of the currently selected square *)

  mutable region_description : string
  (** A description of the currently selected region *)
}

type region_editor_state =
  | MenuActive of menu_state
  (** MenuActive(menu_state) means that we currently see the GUI for the component and region lists, as well as buttons for adding new components
      and regions *)

  | ComponentTopLeft of region * string * string
  (** ComponentTopLeft(region, region_name, component_name) means we are adding a component (rectangle) named [component_name] to [region],
      and we are locating the top left corner of the new component by asking the user to click on a board cell.  *)

  | ComponentBottomRight of region * string * string * position
  (** ComponentBottomRight(region, region_name, component_name, top_left_pos) means we are adding a component (rectangle) named [component_name] to [region],
      and, having already chosen the position [top_left_pos] of its top left corner, we are now choosing the location of the bottom right corner
      of the new component by asking the user to click on a board cell.  *)

type t = region_editor_state ref

let initial_menu_state =
  MenuActive {
    region_list_scroll_index = 0 ;
    focused_region_index = -1 ;
    selected_region_index = -1 ;

    square_list_scroll_index = 0 ;
    focused_square_index = -1 ;
    selected_square_index = 0 ;
    region_description = "" ;
  }

let create () : t = ref initial_menu_state

let width = 600.
let margin = 10.
let height = 690.

let outer_boundary_left = Config.screen_width_f -. width -. margin
let outer_boundary_top = margin

let boundary = Raylib.Rectangle.create outer_boundary_left outer_boundary_top width height

let draw_menu (region_editor_state : t) (menu_state : menu_state) (bp : Board.Blueprint.t) : Raylib.Rectangle.t =
  let open Raylib in

  let center_margin = 20. in
  (** The margin between the left and right halfs of the region editor panel *)

  draw_rectangle_rec boundary Color.black;

  (* Draw "Regions" label *)
  let regions_label_left, regions_label_width, regions_label_bottom =
    let left, top = outer_boundary_left +. margin, outer_boundary_top +. margin in
    let width, height = (width -. (4. *. margin)) /. 2. , 20.0 in
    let boundary = Rectangle.create left top width height in
    Raygui.label boundary "Regions" ;
    left, width, top +. height
  in

  (* Draw "Components" label *)
  let components_label_left =
    let left, top = regions_label_left +. regions_label_width +. center_margin, outer_boundary_top +. margin in
    let width, height = regions_label_width , 20.0 in
    let boundary = Rectangle.create left top width height in
    Raygui.label boundary "Components" ;
    left
  in

  let region_names = Board.Blueprint.region_names bp in

  (* Draw region list *)
  let region_list_top, region_list_bottom, region_list_width, region_list_height, selected_region =
    let boundary_left = regions_label_left in
    let boundary_top = regions_label_bottom +. margin in
    let boundary_width = (width -. (4. *. margin)) /. 2. in
    let boundary_height = 450.0 in
    let boundary = Rectangle.create boundary_left boundary_top boundary_width boundary_height in
    let selected_index, focused_index, scroll_index =
      Raygui.list_view_ex
        boundary
        region_names
        menu_state.focused_region_index
        menu_state.region_list_scroll_index
        menu_state.selected_region_index
    in
    let selected_region =
      match selected_index with
      | -1 ->
        menu_state.region_description <- "";
        None
      | n ->
        let selected_region_name = List.nth region_names selected_index in
        let region = Board.Blueprint.region bp selected_region_name in
        Some(selected_region_name, region)
    in
    menu_state.region_list_scroll_index <- scroll_index;
    menu_state.focused_region_index <- focused_index;
    menu_state.selected_region_index <- selected_index;
    boundary_top, boundary_top +. boundary_height, boundary_width, boundary_height, selected_region
  in

  (* draw region list buttons *)
  let new_region_pressed, del_region_pressed, region_buttons_bottom =
    (* draw new region button *)
    let new_region_left, new_region_top = outer_boundary_left +. margin, region_list_bottom +. margin in
    let new_region_width, new_region_height = (region_list_width /. 2.) -. (margin /. 2.), 30. in
    let add_boundary = Rectangle.create new_region_left new_region_top new_region_width new_region_height in
    Raygui.set_style (Default `Text_size) 18;
    let new_pressed = Raygui.button add_boundary "New Region" in

    let del_region_left, del_region_top = new_region_left +. new_region_width +. margin, new_region_top in
    let del_region_width, del_region_height = new_region_width, new_region_height in
    let del_boundary = Rectangle.create del_region_left del_region_top del_region_width del_region_height in
    Raygui.set_style (Default `Text_size) 18;
    let del_pressed = Raygui.button del_boundary "Delete Region" in

    new_pressed, del_pressed, (del_region_top +. del_region_height)
  in

  if new_region_pressed then
    begin
      match GuiTools.get_new_name "Enter new region name" (fun name -> (List.mem name region_names)) with
      | Some(new_region_name) ->
        Board.Blueprint.add_region bp new_region_name { description = "" ; components = StringMap.empty };
        menu_state.selected_region_index <-
          Option.get (List.find_index ((=) new_region_name) (Board.Blueprint.region_names bp))
      | None ->
        ()
    end;

  (* Draw components (the squares that make up the region) list for the currently selected region *)
  let components_list_bottom =
    let left, top = components_label_left, region_list_top in
    let width, height = region_list_width, region_list_height in
    let boundary = Rectangle.create left top width height in
    let selected_index, focused_index, scroll_index =
      match selected_region with
      | Some(_, region) ->
        let component_names = List.map fst (StringMap.to_list region.components) in
        Raygui.list_view_ex
          boundary
          component_names
          menu_state.focused_square_index
          menu_state.square_list_scroll_index
          menu_state.selected_square_index
      | None ->
        Raygui.list_view_ex
          boundary
          []
          menu_state.focused_square_index
          menu_state.square_list_scroll_index
          menu_state.selected_square_index
    in
    menu_state.square_list_scroll_index <- scroll_index;
    menu_state.focused_square_index <- focused_index;
    menu_state.selected_square_index <- selected_index;
  in

  let new_component_pressed, del_component_pressed =
    (* draw new region button *)
    let new_left, new_top = components_label_left, region_list_bottom +. margin in
    let new_width, new_height = (region_list_width /. 2.) -. (margin /. 2.), 30. in
    let add_boundary = Rectangle.create new_left new_top new_width new_height in
    Raygui.set_style (Default `Text_size) 18;
    let new_pressed = Raygui.button add_boundary "New Component" in

    let del_region_left, del_region_top = new_left +. new_width +. margin, new_top in
    let del_region_width, del_region_height = new_width, new_height in
    let del_boundary = Rectangle.create del_region_left del_region_top del_region_width del_region_height in
    Raygui.set_style (Default `Text_size) 18;
    let del_pressed = Raygui.button del_boundary "Del Component" in

    new_pressed, del_pressed
  in

  begin match new_component_pressed, selected_region with
  | true, Some(region_name, region) ->
    let component_num = Seq.find (fun n -> not @@ StringMap.mem (Int.to_string n) region.components) (Seq.ints 0) in
    let component_name = Int.to_string (Option.get component_num) in
    region_editor_state := ComponentTopLeft(region, region_name, component_name)
  | _, _ ->
    ()
  end;

  (* Draw "region description" label *)
  let description_label_bottom =
    let left, top = outer_boundary_left +. margin, region_buttons_bottom +. 20. in
    let width, height = width, 20. in
    let boundary = Rectangle.create left top width height in
    Raygui.label boundary "Region description" ;
    top +. height
  in

  (* Draw "region description" edit box *)
  begin
    let left, top = outer_boundary_left +. margin, description_label_bottom +. 10. in
    let width, height = width -. (2. *. margin), 100. in
    let boundary = Rectangle.create left top width height in
    begin match selected_region with
    | Some(_, region) ->
      let text, _ = Raygui.text_box_multi boundary region.description true in
      region.description <- text
    | None ->
      let _, _ = Raygui.text_box_multi boundary "" false in
      ()
    end
  end;
  boundary

let draw_top_left (region_editor : t)
                  (region_name : string)
                  (region : region)
                  (component_name : string) : Raylib.Rectangle.t =
  (** Draw the region editor's state when choosing the top-left cell of a new component *)
  let open Raylib in
  draw_rectangle_rec boundary Color.black;

  (* TODO: maybe make the grid cell we are currently hovering over 'highlight' *)
  let text_left = outer_boundary_left +. margin in
  let text_top = outer_boundary_top +. margin in

  (* "Choose the component's top-left board cell by clicking it" *)
  let old_text_color = Raygui.get_style (Default `Text_color_normal) in
  Raygui.set_style (Default `Text_size) 18;
  Raygui.set_style (Default `Text_color_normal) 0xFFFFFFFF;
  Raygui.label (Rectangle.create text_left text_top width 20.) (Printf.sprintf "Creating component %s for region %s" component_name region_name);
  Raygui.label (Rectangle.create text_left (text_top +. 25.) width 20.) "Choose the component's top-left board cell by clicking it";
  Raygui.set_style (Default `Text_color_normal) old_text_color;
  boundary


(** TODO: we need to make the mouse position an argument here so that we can draw a prospective rectangle component as the hover the mouse around
          or we could just get it from the raylib API *)
let draw_bottom_right (region_editor : t)
                      (region_name : string)
                      (region : region)
                      (component_name : string)
                      (top_left_cell_pos : position)
                      (camera_pos : vec2)
                      (scale : float)
                      (mouse_pos : vec2) : Raylib.Rectangle.t =

  (** Draw the region editor's state when choosing the bottom-right cell of a new component *)
  let open Raylib in
  draw_rectangle_rec boundary Color.black;

  let text_left = outer_boundary_left +. margin in
  let text_top = outer_boundary_top +. margin in

  let bottom_right_cell_pos = world_pos_to_cell_pos camera_pos scale mouse_pos in

  let component_top_left = boardpos_top_left camera_pos scale top_left_cell_pos in
  let component_bottom_right = boardpos_bottom_right camera_pos scale bottom_right_cell_pos in

  let diagonal = component_bottom_right ^- component_top_left in

  Raylib.draw_circle_v component_top_left 5. (Color.create 200 100 100 100);

  begin if (Vector2.x diagonal) > 0. && (Vector2.y diagonal) > 0. then
    let region_rect =
      Rectangle.create
        (Vector2.x component_top_left)
        (Vector2.y component_top_left)
        (Vector2.x diagonal)
        (Vector2.y diagonal);
    in
    Raylib.draw_rectangle_rec region_rect (Color.create 20 100 100 100);
  end;
  let old_text_color = Raygui.get_style (Default `Text_color_normal) in
  Raygui.set_style (Default `Text_size) 18;
  Raygui.set_style (Default `Text_color_normal) 0xFFFFFFFF;
  Raygui.label (Rectangle.create text_left text_top width 20.) (Printf.sprintf "Creating component %s for region %s" component_name region_name);
  Raygui.label (Rectangle.create text_left (text_top +. 25.) width 20.) "Choose the component's bottom-right board cell by clicking it";
  Raygui.set_style (Default `Text_color_normal) old_text_color;
  boundary

let draw (region_editor : t) (bp : Board.Blueprint.t) (camera_pos : vec2) (scale : float) (mouse_pos : vec2) : Raylib.Rectangle.t =
  match !region_editor with
  | MenuActive(menu_state) ->
    draw_menu region_editor menu_state bp
  | ComponentTopLeft(region, region_name, component_name) ->
    draw_top_left region_editor region_name region component_name
  | ComponentBottomRight(region, region_name, component_name, top_left_cell_pos) ->
    draw_bottom_right region_editor region_name region component_name top_left_cell_pos camera_pos scale mouse_pos

let click_left (region_editor : t) (bp : Board.Blueprint.t) (cursor_cell_pos : position) (camera_pos : vec2) (scale : float) =
  match !region_editor with
  | MenuActive(_) ->
    ()
  | ComponentTopLeft(region, region_name, component_name) ->
    region_editor := ComponentBottomRight(region, region_name, component_name, cursor_cell_pos);
  | ComponentBottomRight(region, region_name, component_name, top_left_pos) ->
    let {x = cursor_x ; y = cursor_y} = cursor_cell_pos in
    let {x = top_left_x ; y = top_left_y } = top_left_pos in
    if top_left_x <= cursor_x && top_left_y <= cursor_y then
      let open Board in
      let new_component = { left = top_left_x ; top = top_left_y ; right = cursor_x ; bottom = cursor_y } in
      region.components <- StringMap.add component_name new_component region.components;
      region_editor := initial_menu_state