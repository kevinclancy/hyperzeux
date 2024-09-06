module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

type static_object = {
  (* The texture that depicts this object type *)
  texture_name : string ;
  (* Whether agents can move into the same cell and occlude this object type *)
  traversable : bool ;
  (* The name of the static object type *)
  name : string ;
}

type position = {
  (* The column of the cell position *)
  x : int ;
  (* The row of the cell position *)
  y : int ;
}