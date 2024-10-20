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

let name = "patroller"

let preview_texture_name = "person_south_recon.png"

let preview_color = Raylib.Color.white

let create (agent_name : string) (pos : position) (color : Raylib.Color.t) : Agent.t =
  let scripts = {
    Agent.empty_scripts with
      (* Initially start patrolling in a 2x2 square *)
      initial = Some (function (me : Agent.t) ->
        while true do
          walk_north me;
          walk_north me;

          walk_east me;
          walk_east me;

          walk_south me;
          walk_south me;

          walk_west me;
          walk_west me;
        done
      ) ;
  }
  in
  Agent.create agent_name scripts pos color (TextureMap.get "person_south_recon.png") ~speed:0.3

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
