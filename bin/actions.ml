
type action =
  | Walk of int * int
  | Wait
  (** An intentional action preformed by the agent to forfeit a turn *)
  | Idle
  (** An unintentional action forfeiting the agent's turn because it is too slow or idling *)


type action_result =
  | Success
  | Failure

type _ Effect.t += Act : action -> action_result Effect.t

let walk (dx : int) (dy : int) =
  let finished = ref false in
  while not !finished do
    begin
      match Effect.perform @@ Act (Walk (dx, dy)) with
      | Success ->
        finished := true
      | Failure ->
        ()
    end
  done

let walk_north () = walk 0 (-1)
let walk_east () = walk 1 0
let walk_south () = walk 0 1
let walk_west () = walk (-1) 0

let wait (n : int) =
  for i = 1 to n do
    ignore (Effect.perform @@ Act Wait)
  done

let print_action (a : action) : unit =
  match a with
  | Walk (dx, dy) ->
    Printf.printf "%d %d\n" dx dy
  | Wait ->
    Printf.printf "wait\n"
  | Idle ->
    Printf.printf "idle\n"