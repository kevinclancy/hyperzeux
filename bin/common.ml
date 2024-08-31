module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

type static_object = {
  texture_name : string ;
  traversable : bool ;
}

type position = {
  x : int ;  (** The column of the cell position *)
  y : int ;  (** The row of the cell position *)
}