
module type OrdMapArgs = sig
  type a
  (** The type of the elements stored in the map *)

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

module Make : functor (S : OrdMapArgs) -> (OrdMap with type a = S.a)
