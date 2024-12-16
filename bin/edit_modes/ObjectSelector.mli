open Common

(** An object selector *)
type t

val create : unit -> t

val get_curr_name : t -> string

(** [get_curr_color selector] Returns the currently selected color *)
val get_curr_color : t -> Raylib.Color.t

(** [set_obj selector obj] sets [obj] as the current object *)
val set_obj : t -> static_object -> unit

val next_obj : t -> unit

val prev_obj : t -> unit

val draw : t -> unit

val instantiate : t -> Board.Blueprint.t -> position -> unit