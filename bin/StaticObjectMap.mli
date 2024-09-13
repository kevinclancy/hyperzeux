open Common

(** [add obj] Adds [obj] to the static object map *)
val add : static_object -> unit

(** [get name] Gets the static object named [name] from the map *)
val get : string -> static_object

(** [get_first_object ()] Gets the first static object in the order added *)
val get_first_object : unit -> static_object

(** [get_next_object obj] Gets the object after [obj] in the order added.
    The object after the last is the first. *)
val get_next_object : static_object -> static_object

(** [get_next_object obj] Gets the object before [obj] in the order added.
    The object before the first is the last. *)
val get_prev_object : static_object -> static_object

(** [search s] is a list of all static objects whose names contains [s],
    or empty if s has fewer than three characters. *)
val search : string -> static_object list