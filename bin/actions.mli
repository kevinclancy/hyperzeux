
type action =
  | Walk of int * int (** [walk xoffset yoffset] *)
  | Wait

type _ Effect.t += Act : action -> unit Effect.t

val walk_north : unit -> unit
val walk_east : unit -> unit
val walk_south : unit -> unit
val walk_west : unit -> unit

val wait : int -> unit
(** [wait n] waits for [n] ticks *)

val print_action : action -> unit