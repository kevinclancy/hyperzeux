module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type static_object = {
  (* The texture that depicts this object type *)
  texture_name : string ;
  (* Whether agents can move into the same cell and occlude this object type *)
  traversable : bool ;
  (* The name of the static object type *)
  name : string ;
}

type pre_position = {
  x : int ;
  (** The column of the cell position *)
  y : int ;
  (** The row of the cell position *)
}

type position = {
  layer : string ;
  (** The layer this point is in *)
  x : int ;
  (** The column of the cell position *)
  y : int ;
  (** The row of the cell position *)
}

type direction =
  | North
  | East
  | South
  | West

type vec2 = Raylib.Vector2.t

type camera_transform = {
  layer : string ;
  (** the name of the layer to transform *)

  pos : vec2 ;
  (** pixel-space position of the camera relative to the top-left of the board *)

  scale : float
  (** pixel-space scale to display the board at *)
}

let char_width_f = Float.of_int @@ Config.char_width
let char_height_f = Float.of_int @@ Config.char_height

let rect = Raylib.Rectangle.create

let vec2 = Raylib.Vector2.create

let (^*) = Raylib.Vector2.scale

let (^+) = Raylib.Vector2.add

let (^-) = Raylib.Vector2.subtract

(** Get the board cell position that the mouse is currently hovering over *)
let get_mouse_boardpos (camera_pos : Raylib.Vector2.t) (scale : float) : pre_position =
  let open Raylib in
  let mouse_pos = Raylib.get_mouse_position () in
  let x = Int.of_float @@ ((Vector2.x mouse_pos) +. (Vector2.x camera_pos)) /. (scale *. Config.char_width_f) in
  let y = Int.of_float @@ ((Vector2.y mouse_pos) +. (Vector2.y camera_pos)) /. (scale *. Config.char_height_f) in
  { x ; y }

(** @param cell_pos : position : the discrete board space coordinate of a cell
    @return vec2 : the continuous board-space coordinate of the center of the cell at [cell_pos] *)
let disc_to_cont (cell_pos : position) : vec2 =
  let { x = cell_col ; y = cell_row ; layer = _ } = cell_pos in
  Raylib.Vector2.create
    (((Float.of_int cell_col) +. 0.5) *. Config.char_width_f)
    (((Float.of_int cell_row) +. 0.5) *. Config.char_height_f)

(** [boardpos_top_left camera_pos scale cell_pos] Get the screen position of the top-left
    corner of the board cell identified by [cell_pos] *)
let boardpos_top_left (camera_pos : Raylib.Vector2.t) (scale : float) (cell_pos : pre_position) : Raylib.Vector2.t =
  let {x = cell_col ; y = cell_row} : pre_position = cell_pos in
  let world_pos =
    vec2 ((Float.of_int cell_col) *. Config.char_width_f) ((Float.of_int cell_row) *. Config.char_height_f)
  in
  (world_pos ^* scale) ^- camera_pos

(** [boardpos_bottom_right camera_pos scale cell_pos] Get the screen position of the bottom-right
    corner of the board cell identified by [cell_pos] *)
let boardpos_bottom_right (camera_pos : Raylib.Vector2.t) (scale : float) (cell_pos : pre_position) : Raylib.Vector2.t =
  boardpos_top_left camera_pos scale { x = cell_pos.x + 1; y = cell_pos.y + 1 }

let world_pos_to_cell_pos (camera_pos : vec2) (scale : float) (_pos_to_convert : vec2) : pre_position =
  let open Raylib in
  let mouse_pos = Raylib.get_mouse_position () in
  let x = Int.of_float @@ ((Vector2.x mouse_pos) +. (Vector2.x camera_pos)) /. (scale *. (Float.of_int @@ Config.char_width)) in
  let y = Int.of_float @@ ((Vector2.y mouse_pos) +. (Vector2.y camera_pos)) /. (scale *. (Float.of_int @@ Config.char_height)) in
  {x;y}