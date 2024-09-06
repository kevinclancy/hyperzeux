open Common

type iterator = int

(* Maps each name to a static object of that name *)
let name_to_obj = ref StringMap.empty

(* Maps each order identifier to a static object *)
let ord_to_obj = ref IntMap.empty

let name_to_ord = ref StringMap.empty

(** The maximum ord allocated so far, or -1 if the map is empty  *)
let max_ord = ref (-1)

let contains_name (name : string) : bool =
  StringMap.mem name !name_to_obj

let add (obj : static_object) : unit =
  assert (not @@ contains_name obj.name);
  name_to_obj := StringMap.add obj.name obj !name_to_obj;
  max_ord := !max_ord + 1;
  ord_to_obj := IntMap.add !max_ord obj !ord_to_obj;
  name_to_ord := StringMap.add obj.name !max_ord !name_to_ord

let get (static_obj_name : string) : static_object =
  StringMap.find static_obj_name !name_to_obj

let get_first_object () : static_object =
  (* don't get an object when the map is empty *)
  assert (!max_ord <> -1);
  IntMap.find 0 !ord_to_obj

let get_next_object (obj : static_object) =
  assert (!max_ord <> -1);
  let ord = StringMap.find obj.name !name_to_ord in
  let ord' = (ord + 1) mod (!max_ord + 1) in
  IntMap.find ord' !ord_to_obj

  let get_prev_object (obj : static_object) =
    assert (!max_ord <> -1);
    let ord = StringMap.find obj.name !name_to_ord in
    let ord' = if ord - 1 < 0 then !max_ord else ord - 1 in
    IntMap.find ord' !ord_to_obj
