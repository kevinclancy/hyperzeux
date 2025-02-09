open Common

include OrdMap.Make(
  struct
    type a = static_object
    let get_name (x : static_object) : string =
      x.name
  end
)
