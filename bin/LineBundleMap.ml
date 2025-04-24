open Common

type line_drawing_bundle = {
  (** static object names for each line configuration *)

  name : string ;
  (** The name of the bundle *)

  nesw : string ;
  (** static object name to connect adjacent cells in all four directions *)

  nesx : string ;
  (** static object name to connect to adjacent cells in north, east, and south directions *)

  nexw : string ;
  nexx : string ;
  nxws : string ;
  nxsx : string ;
  nxxw : string ;
  xesw : string ;
  xesx : string ;
  xexw : string ;
  xxsw : string ;
  xxxx : string ;
}

let all_piece_names (bundle : line_drawing_bundle) : StringSet.t =
  let { nesw ; nesx ; nexw ; nexx ; nxws ; nxsx ; nxxw ; xesw ; xesx ; xexw ; xxsw ; xxxx } = bundle in
  StringSet.of_list [ nesw ; nesx ; nexw ; nexx ; nxws ; nxsx ; nxxw ; xesw ; xesx ; xexw ; xxsw ; xxxx ]

include OrdMap.Make(
  struct
    type a = line_drawing_bundle
    let get_name (x : line_drawing_bundle) : string =
      x.name
  end
)