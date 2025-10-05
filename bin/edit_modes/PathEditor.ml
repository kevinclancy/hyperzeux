open Common
open Path

type menu_state = {
  mutable path_list_scroll_index : int ;
  (** The path list's scrollbar position *)

  mutable focused_path_index : int ;
  (** index of path name that mouse is hovering over (or -1 if not over an item) *)

  mutable selected_path_index : int ;
  (** The index of the currently selected path *)

  mutable node_list_scroll_index : int ;
  (** The node list's scrollbar position *)

  mutable focused_node_index : int ;
  (** index of node that mouse is hovering over (or -1 if not over an item) *)

  mutable selected_node_index : int ;
  (** The index of the currently selected node *)

  mutable path_description : string
  (** A description of the currently selected path *)
}

type path_editor_state =
  | MenuActive of menu_state
  (** MenuActive(menu_state) means that we currently see the GUI for the path and node lists, as well as buttons for adding new paths
      and nodes *)

  | NodeSelection of path * string * int option
  (** NodeSelection(path, path_name, insert_index) means we are adding a node to [path] at position [insert_index],
      and we are asking the user to click on a board cell to select the node position. If insert_index is None, append to end. *)

type t = path_editor_state ref

let initial_menu_state =
  MenuActive {
    path_list_scroll_index = -1 ;
    focused_path_index = -1 ;
    selected_path_index = -1 ;

    node_list_scroll_index = -1 ;
    focused_node_index = -1 ;
    selected_node_index = -1 ;
    path_description = "" ;
  }

let create () : t = ref initial_menu_state

let width = 600.
let margin = 10.
let height = 690.

let outer_boundary_left = Config.screen_width_f -. width -. margin
let outer_boundary_top = margin

let boundary = Raylib.Rectangle.create outer_boundary_left outer_boundary_top width height

let draw_menu (path_editor_state : t) (menu_state : menu_state) (edit_state : Board.Blueprint.edit_state) : Raylib.Rectangle.t =
  let open Raylib in

  let center_margin = 20. in
  (** The margin between the left and right halfs of the path editor panel *)

  draw_rectangle_rec boundary Color.black;

  (* Draw "Paths" label *)
  let paths_label_left, paths_label_width, paths_label_bottom =
    let left, top = outer_boundary_left +. margin, outer_boundary_top +. margin in
    let width, height = (width -. (4. *. margin)) /. 2. , 20.0 in
    let boundary = Rectangle.create left top width height in
    Raygui.label boundary "Paths" ;
    left, width, top +. height
  in

  (* Draw "Nodes" label *)
  let nodes_label_left =
    let left, top = paths_label_left +. paths_label_width +. center_margin, outer_boundary_top +. margin in
    let width, height = paths_label_width , 20.0 in
    let boundary = Rectangle.create left top width height in
    Raygui.label boundary "Nodes" ;
    left
  in

  let path_names = Board.Blueprint.path_names edit_state in

  (* Draw path list *)
  let path_list_top, path_list_bottom, path_list_width, path_list_height, selected_path =
    let boundary_left = paths_label_left in
    let boundary_top = paths_label_bottom +. margin in
    let boundary_width = (width -. (4. *. margin)) /. 2. in
    let boundary_height = 450.0 in
    let boundary = Rectangle.create boundary_left boundary_top boundary_width boundary_height in
    let selected_index, focused_index, scroll_index =
      Raygui.list_view_ex
        boundary
        path_names
        menu_state.focused_path_index
        menu_state.path_list_scroll_index
        menu_state.selected_path_index
    in
    let selected_path =
      match selected_index with
      | -1 ->
        menu_state.path_description <- "";
        None
      | n ->
        let selected_path_name = List.nth path_names selected_index in
        let path = Board.Blueprint.get_path edit_state selected_path_name in
        if n <> menu_state.selected_path_index then menu_state.selected_node_index <- -1;
        Some(selected_path_name, path)
    in
    menu_state.path_list_scroll_index <- scroll_index;
    menu_state.focused_path_index <- focused_index;
    menu_state.selected_path_index <- selected_index;
    boundary_top, boundary_top +. boundary_height, boundary_width, boundary_height, selected_path
  in

  (* draw path list buttons *)
  let new_path_pressed, del_path_pressed, path_buttons_bottom =
    (* draw new path button *)
    let new_path_left, new_path_top = outer_boundary_left +. margin, path_list_bottom +. margin in
    let new_path_width, new_path_height = (path_list_width /. 2.) -. (margin /. 2.), 30. in
    let add_boundary = Rectangle.create new_path_left new_path_top new_path_width new_path_height in
    Raygui.set_style (Default `Text_size) 18;
    let new_pressed = Raygui.button add_boundary "New Path" in

    let del_path_left, del_path_top = new_path_left +. new_path_width +. margin, new_path_top in
    let del_path_width, del_path_height = new_path_width, new_path_height in
    let del_boundary = Rectangle.create del_path_left del_path_top del_path_width del_path_height in
    Raygui.set_style (Default `Text_size) 18;
    let del_pressed = Raygui.button del_boundary "Delete Path" in

    new_pressed, del_pressed, (del_path_top +. del_path_height)
  in

  if new_path_pressed then
    begin
      match GuiTools.get_new_name "Enter new path name" (fun name -> (List.mem name path_names)) with
      | Some(new_path_name) ->
        Board.Blueprint.add_path edit_state new_path_name { description = "" ; nodes = [] };
        menu_state.selected_path_index <-
          Option.get (List.find_index ((=) new_path_name) (Board.Blueprint.path_names edit_state))
      | None ->
        ()
    end;

  if del_path_pressed then
      begin
        Option.iter (fun (name, path) -> Board.Blueprint.del_path edit_state name) selected_path;
        menu_state.selected_path_index <- -1;
        menu_state.selected_node_index <- -1
      end;

  (* Draw nodes (the positions that make up the path) list for the currently selected path *)
  let nodes_list_bottom, selected_node_index =
    let left, top = nodes_label_left, path_list_top in
    let width, height = path_list_width, path_list_height in
    let boundary = Rectangle.create left top width height in
    let selected_index, focused_index, scroll_index, node_strings =
      match selected_path with
      | Some(_, path) ->
        let node_strings = List.mapi (fun i ({x;y} : pre_position) -> Printf.sprintf "%d: (%d,%d)" i x y) path.nodes in
        let selected_ind, focused_ind, scroll_ind =
          Raygui.list_view_ex
            boundary
            node_strings
            menu_state.focused_node_index
            menu_state.node_list_scroll_index
            menu_state.selected_node_index
        in
        selected_ind, focused_ind, scroll_ind, node_strings
      | None ->
        let (a,b,c) =
          Raygui.list_view_ex
            boundary
            []
            menu_state.focused_node_index
            menu_state.node_list_scroll_index
            menu_state.selected_node_index
        in
        a,b,c,[]
    in
    menu_state.node_list_scroll_index <- scroll_index;
    menu_state.focused_node_index <- focused_index;
    menu_state.selected_node_index <- selected_index;
    path_list_bottom, selected_index
  in

  let new_node_pressed, del_node_pressed =
    (* draw new node button *)
    let new_left, new_top = nodes_label_left, path_list_bottom +. margin in
    let new_width, new_height = (path_list_width /. 2.) -. (margin /. 2.), 30. in
    let add_boundary = Rectangle.create new_left new_top new_width new_height in
    Raygui.set_style (Default `Text_size) 18;
    let new_pressed = Raygui.button add_boundary "New Node" in

    let del_node_left, del_node_top = new_left +. new_width +. margin, new_top in
    let del_node_width, del_node_height = new_width, new_height in
    let del_boundary = Rectangle.create del_node_left del_node_top del_node_width del_node_height in
    Raygui.set_style (Default `Text_size) 18;
    let del_pressed = Raygui.button del_boundary "Del Node" in

    new_pressed, del_pressed
  in

  if del_node_pressed then
    begin match selected_path, selected_node_index with
    | Some(path_name, path), n when n >= 0 ->
      path.nodes <- List.filteri (fun i _ -> i <> n) path.nodes;
      menu_state.selected_node_index <- -1
    | _ ->
      ()
    end;

  begin match new_node_pressed, selected_path with
  | true, Some(path_name, path) ->
    path_editor_state := NodeSelection(path, path_name, None)
  | _, _ ->
    ()
  end;

  (* Draw "path description" label *)
  let description_label_bottom =
    let left, top = outer_boundary_left +. margin, path_buttons_bottom +. 20. in
    let width, height = width, 20. in
    let boundary = Rectangle.create left top width height in
    Raygui.label boundary "Path description" ;
    top +. height
  in

  (* Draw "path description" edit box *)
  begin
    let left, top = outer_boundary_left +. margin, description_label_bottom +. 10. in
    let width, height = width -. (2. *. margin), 100. in
    let boundary = Rectangle.create left top width height in
    begin match selected_path with
    | Some(_, path) ->
      let text, _ = Raygui.text_box_multi boundary path.description true in
      path.description <- text
    | None ->
      let _, _ = Raygui.text_box_multi boundary "" false in
      ()
    end
  end;
  boundary

let draw_node_selection (path_editor : t)
                        (path_name : string)
                        (path : path)
                        (insert_index : int option) : Raylib.Rectangle.t =
  (** Draw the path editor's state when choosing a node position for a new node *)
  let open Raylib in
  draw_rectangle_rec boundary Color.black;

  let text_left = outer_boundary_left +. margin in
  let text_top = outer_boundary_top +. margin in

  let old_text_color = Raygui.get_style (Default `Text_color_normal) in
  Raygui.set_style (Default `Text_size) 18;
  Raygui.set_style (Default `Text_color_normal) 0xFFFFFFFF;
  Raygui.label (Rectangle.create text_left text_top width 20.) (Printf.sprintf "Adding node to path %s" path_name);
  Raygui.label (Rectangle.create text_left (text_top +. 25.) width 20.) "Click a board cell to select the node position";
  Raygui.set_style (Default `Text_color_normal) old_text_color;
  boundary

let draw (path_editor : t) (edit_state : Board.Blueprint.edit_state) (camera_pos : vec2) (scale : float) (mouse_pos : vec2) : Raylib.Rectangle.t =
  match !path_editor with
  | MenuActive(menu_state) ->
    draw_menu path_editor menu_state edit_state
  | NodeSelection(path, path_name, insert_index) ->
    draw_node_selection path_editor path_name path insert_index

let click_left (path_editor : t) (edit_state : Board.Blueprint.edit_state) (cursor_cell_pos : pre_position) (camera_pos : vec2) (scale : float) =
  match !path_editor with
  | MenuActive(menu_state) ->
    (* If a path is selected, add node to end of that path *)
    begin match menu_state.selected_path_index with
    | -1 -> ()
    | idx ->
      let path_names = Board.Blueprint.path_names edit_state in
      let selected_path_name = List.nth path_names idx in
      let path = Board.Blueprint.get_path edit_state selected_path_name in
      path.nodes <- path.nodes @ [cursor_cell_pos]
    end
  | NodeSelection(path, path_name, insert_index) ->
    begin match insert_index with
    | None ->
      path.nodes <- path.nodes @ [cursor_cell_pos]
    | Some(idx) ->
      let rec insert_at i lst elem =
        match lst with
        | [] -> [elem]
        | h :: t ->
          if i = 0 then elem :: h :: t
          else h :: (insert_at (i - 1) t elem)
      in
      path.nodes <- insert_at idx path.nodes cursor_cell_pos
    end;
    path_editor := initial_menu_state
