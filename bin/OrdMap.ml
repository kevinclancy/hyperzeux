open Common

module type OrdMapArgs = sig
  type a
  (** The type of the map's elements *)

  val get_name : a -> string
  (** Gets the name of an element, used for listing and ordering the element *)
end

module type OrdMap = sig
  type a
  (** The type of elements stored in the map *)

  val add : a -> unit
  (** [add obj] Adds element [obj] to [map].

      Precondition: map does not already contain an element whose name
      matches [obj]. *)


  val get : string -> a
  (** [get name] Gets the element named [name] from the map.

      Precondition: map contains an element with name [name] *)

  val try_get : string -> a option
  (** [try_get name] Gets the element named [name] from the map, or returns None if
      the map contains no such item. *)

  val get_all_names : unit -> string list
  (** Return names of all elements *)

  val get_first_elem : unit -> a
  (** [get_first_elem ()] Gets the first element that was added to the map.

      Precondition: [map] is non-empty *)

  val get_next_elem : a -> a
  (** [get_next_elem elem] Gets the element added after [elem], or the first added
      element if [elem] is the last added element.

      Precondition: [map] is non-empty *)

  val get_prev_elem : a -> a
  (** [get_next_elem elem] Gets the element added before [elem],
      or the last element if [elem] is the first. *)

  val search : string -> a list
  (** [search s] is a list of all elements of the map whose names contains [s],
      or empty if s has fewer than three characters. *)
end

module Make (S : OrdMapArgs) : (OrdMap with type a = S.a) = struct
  type a = S.a

  type t = {
    (* Maps each name to an element of that name *)
    name_to_elem : (S.a StringMap.t) ref ;
    (* Maps n to the nth element added to this map *)
    ord_to_elem : (S.a IntMap.t) ref ;
    (* Maps element with name [name] to [n] where the element
      named [name] was the [n]th element added to this map *)
    name_to_ord : (int StringMap.t) ref ;
    (* The maximum ord allocated so far, or -1 if the map is empty  *)
    max_ord : int ref
  }

  let name_to_elem = ref StringMap.empty
  let ord_to_elem = ref IntMap.empty
  let name_to_ord = ref StringMap.empty
  let max_ord = ref (-1)

  let contains_name (name : string) : bool =
    StringMap.mem name !name_to_elem

  let add (elem : S.a) : unit =
    let name = S.get_name elem in
    assert (not @@ contains_name name);
    name_to_elem := StringMap.add name elem !name_to_elem;
    max_ord := !max_ord + 1;
    ord_to_elem := IntMap.add !max_ord elem !ord_to_elem;
    name_to_ord := StringMap.add name !max_ord !name_to_ord

  let get (elem_name : string) : S.a =
    StringMap.find elem_name !name_to_elem

  let try_get (elem_name : string) : S.a option =
    StringMap.find_opt elem_name !name_to_elem

  let get_all_names () : string list =
    List.map fst (StringMap.bindings !name_to_elem)

  let get_first_elem () : 'a =
    (** har dee har har *)

    (* don't get an object when the map is empty *)
    assert (!max_ord <> -1);
    IntMap.find 0 !ord_to_elem

  let get_next_elem (elem : 'a) =
    assert (!max_ord <> -1);
    let ord = StringMap.find (S.get_name elem) !name_to_ord in
    let ord' = (ord + 1) mod (!max_ord + 1) in
    IntMap.find ord' !ord_to_elem

  let get_prev_elem (elem : 'a) =
    assert (!max_ord <> -1);
    let ord = StringMap.find (S.get_name elem) !name_to_ord in
    let ord' = if ord - 1 < 0 then !max_ord else ord - 1 in
    IntMap.find ord' !ord_to_elem

  let search (text : string) : 'a list =
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
        (IntMap.to_list @@ IntMap.filter (fun _ e -> contains_text (S.get_name e)) !ord_to_elem)
end