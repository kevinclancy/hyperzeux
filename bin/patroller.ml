open Actions

let run (agent : Agent.t) =
  ignore agent;
  while true do
    walk_north ();
    walk_north ();
    walk_south ();
    walk_south ();
  done

let create (pos : int * int) =
  Agent.create run pos

(* let resume () =
  let open Effect.Deep in
  let f () =
    match !cont with
    | Some(k) ->
      continue k ()
    | None ->
      run ();
      failwith "unreachable"
  in
  match_with
    f
    ()
    { retc = failwith "agent script returned a value" ;
      exnc = raise ;
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Act action ->
          Some (fun (k : (a, _) continuation) -> cont := Some(k); action)
        | _ ->
          None
    }
 *)
