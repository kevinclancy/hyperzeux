
type action =
  | Walk of int * int
  | Wait

type _ Effect.t += Act : action -> unit Effect.t

let walk_north () = Effect.perform @@ Act (Walk (0, -1))
let walk_east () = Effect.perform @@ Act (Walk (1, 0))
let walk_south () = Effect.perform @@ Act (Walk (0, 1))
let walk_west () = Effect.perform @@ Act (Walk (-1, 0))

let wait (n : int) =
  for i = 1 to n do
    Effect.perform @@ Act Wait;
  done

let print_action (a : action) : unit =
  match a with
  | Walk (dx, dy) ->
    Printf.printf "%d %d\n" dx dy
  | Wait ->
    Printf.printf "wait\n"