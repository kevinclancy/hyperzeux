open Common

type 'a t = {
  (* Gets the name of an element *)
  get_name : 'a -> string ;
  (* Maps each name to an element of that name *)
  name_to_elem : ('a StringMap.t) ref ;
  (* Maps n to the nth element added to this map *)
  ord_to_elem : ('a IntMap.t) ref ;
  (* Maps element with name [name] to [n] where the element
     named [name] was the [n]th element added to this map *)
  name_to_ord : (int StringMap.t) ref ;
  (* The maximum ord allocated so far, or -1 if the map is empty  *)
  max_ord : int ref
}

let create (get_name : 'a -> string) : 'a t =
  {
    get_name ;
    name_to_elem = ref StringMap.empty ;
    ord_to_elem = ref IntMap.empty ;
    name_to_ord = ref StringMap.empty ;
    max_ord = ref (-1)
  }

let contains_name (map : 'a t) (name : string) : bool =
  StringMap.mem name !(map.name_to_elem)

let add (map : 'a t) (elem : 'a) : unit =
  let name = map.get_name elem in
  assert (not @@ contains_name map name);
  map.name_to_elem := StringMap.add name elem !(map.name_to_elem);
  map.max_ord := !(map.max_ord) + 1;
  map.ord_to_elem := IntMap.add !(map.max_ord) elem !(map.ord_to_elem);
  map.name_to_ord := StringMap.add name !(map.max_ord) !(map.name_to_ord)

let get (map : 'a t) (elem_name : string) : 'a =
  StringMap.find elem_name !(map.name_to_elem)

let get_first_elem (map : 'a t) : 'a =
  (* don't get an object when the map is empty *)
  assert (!(map.max_ord) <> -1);
  IntMap.find 0 !(map.ord_to_elem)

let get_next_elem (map : 'a t) (elem : 'a) =
  assert (!(map.max_ord) <> -1);
  let ord = StringMap.find (map.get_name elem) !(map.name_to_ord) in
  let ord' = (ord + 1) mod (!(map.max_ord) + 1) in
  IntMap.find ord' !(map.ord_to_elem)

let get_prev_elem (map : 'a t) (elem : 'a) =
  assert (!(map.max_ord) <> -1);
  let ord = StringMap.find (map.get_name elem) !(map.name_to_ord) in
  let ord' = if ord - 1 < 0 then !(map.max_ord) else ord - 1 in
  IntMap.find ord' !(map.ord_to_elem)

let search (map : 'a t) (text : string) : 'a list =
  (** Does [s] contain [text]? *)
  let contains_text (s : string) : bool =
    let open String in
    let success = ref false in
    for i = 0 to (length s - length text) do
      let match_success = ref true in
      for j = 0 to (length text)-1 do
        if text.[j] <> s.[i + j] then
          match_success := false
        else
          ();
      done;
      if !match_success then success := true else ();
    done;
    !success
  in
  if String.length text < 3 then
    []
  else
    List.map snd @@
      (IntMap.to_list @@ IntMap.filter (fun _ e -> contains_text (map.get_name e)) !(map.ord_to_elem))

