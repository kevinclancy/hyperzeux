
type action =
  | Walk of int * int
  (** [walk xoffset yoffset] The agent intends to move itself along the given offset *)
  | Wait
  (** An intentional action preformed by the agent to forfeit a turn *)
  | Idle
  (** An unintentional action forfeiting the agent's turn because it is too slow or idling *)

type action_result =
  | Success
  | Failure

type _ Effect.t += Act : action -> action_result Effect.t

val walk_north : unit -> unit
val walk_east : unit -> unit
val walk_south : unit -> unit
val walk_west : unit -> unit

val wait : int -> unit
(** [wait n] waits for [n] ticks *)

val print_action : action -> unit