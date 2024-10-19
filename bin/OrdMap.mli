(** A map containing elements of type ['a], which allows us to look up an element
    using either its name or the natural number [n] where the element was the
    [n]th element added to the map.  *)
type 'a t

(** [create get_name] Creates a new OrdMap of element type ['a],
    where [get_name elem] gets the name of an element. *)
val create : ('a -> string) -> 'a t

(** [add map obj] Adds element [obj] to [map].

    Precondition: [map] does not already contain an element whose name
    matches [obj]. *)
val add : 'a t -> 'a -> unit

(** [get map name] Gets the element named [name] from the map.

    Precondition: [map] contains an element with name [name] *)
val get : 'a t -> string -> 'a

(** [get_first_elem map] Gets the first element that was added to [map].

    Precondition: [map] is non-empty *)
val get_first_elem : 'a t -> 'a

(** [get_next_elem elem] Gets the element added after [elem], or the first added
    element if [elem] is the last added element.

    Precondition: [map] is non-empty *)
val get_next_elem : 'a t -> 'a -> 'a

(** [get_next_elem map elem] Gets the element added before [elem],
    or the last element if [elem] is the first. *)
val get_prev_elem : 'a t -> 'a -> 'a

(** [search map s] is a list of all elements of [map] whose names contains [s],
    or empty if s has fewer than three characters. *)
val search : 'a t -> string -> 'a list