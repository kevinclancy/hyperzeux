
type action =
  | WalkNorth
  | WalkEast
  | WalkSouth
  | WalkWest
  | Wait

type _ Effect.t += Act : action -> unit Effect.t

let walk_north () = Effect.perform @@ Act WalkNorth;;

(* let walk_east () = Effect.perform @@ Act WalkEast;; *)

let walk_south () = Effect.perform @@ Act WalkNorth;;