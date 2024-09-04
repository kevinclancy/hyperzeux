open Common

type state =
  (* [NewAgent script_run] where [script_run] may produce [Act] effects  *)
  | NewAgent of (t -> unit)
  (* [RunningAgent cont] where [cont] may produce [Act] effects *)
  | RunningAgent of (unit, Actions.action) Effect.Deep.continuation

and t = {
  name : string ;
  state : state ref ;
  pos : position ref ;
  texture : Raylib.Texture.t ref
};;

let set_pos (agent : t) (pos : position) =
  agent.pos := pos;;

let get_pos (agent : t) : position =
  !(agent.pos);;

let set_texture (agent : t) (texture : Raylib.Texture.t) : unit =
  agent.texture := texture

let get_texture (agent : t) : Raylib.Texture.t =
  !(agent.texture);;

let create (name : string) (script : t -> unit) (pos : position) (texture : Raylib.Texture.t) : t =
  { name ; state = ref (NewAgent script) ; pos = ref pos ; texture = ref texture }

let resume (agent : t) : Actions.action =
  let open Effect.Deep in
  let open Actions in
  match !(agent.state) with
  | RunningAgent k ->
    continue k ()
  | NewAgent run ->
    match_with
      run
      agent
      { retc = (fun _ -> failwith "agent script returned a value") ;
        exnc = raise ;
        effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Act action ->
            Some (function (k : (a, _) continuation) -> agent.state := RunningAgent k; action)
          | _ ->
            None }
  (* let f () =
    match !(agent.state) with
    | RunningAgent k ->
      Printf.printf "hi\n";
      let _ = continue k () in
      Printf.printf "yoyoyo!\n";
      failwith "unreachable"
    | NewAgent run ->
      run agent;
      failwith "unreachable"
  in
  match_with
    f
    ()
    { retc = (fun a -> Actions.print_action a; a) ;
      exnc = raise ;
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | Act action ->
          Some (function (k : (a, _) continuation) -> agent.state := RunningAgent k; action)
        | _ ->
          None
    } *)

let compare (a : t) (b : t) : int =
  String.compare a.name b.name