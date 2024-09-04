open Common

let map = ref StringMap.empty

let add (static_obj_name : string) (obj : static_object) : unit =
  map := StringMap.add static_obj_name obj !map

let get (static_obj_name : string) : static_object =
  StringMap.find static_obj_name !map