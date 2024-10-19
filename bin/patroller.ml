open Common

let walk_north (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_north_recon.png");
  Actions.walk_north ()

let walk_east (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_east_recon.png");
  Actions.walk_east ()

let walk_south (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_south_recon.png");
  Actions.walk_south ()

let walk_west (agent : Agent.t) =
  Agent.set_texture agent (TextureMap.get "person_west_recon.png");
  Actions.walk_west ()

let run (agent : Agent.t) : unit =
  while true do
    walk_north agent;
    walk_north agent;

    walk_east agent;
    walk_east agent;

    walk_south agent;
    walk_south agent;

    walk_west agent;
    walk_west agent;
  done

let name = "patroller"

let create (agent_name : string) (pos : position) : Agent.t =
  Agent.create agent_name run pos (TextureMap.get "person_south_recon.png") ~speed:0.5

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
