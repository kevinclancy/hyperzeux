
type action =
  | WalkNorth
  | WalkEast
  | WalkSouth
  | WalkWest
  | Wait

type _ Effect.t += Act : action -> unit Effect.t

val walk_north : unit -> unit
val walk_south : unit -> unit