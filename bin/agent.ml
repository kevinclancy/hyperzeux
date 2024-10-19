open Common

type state =
  | NewAgent of (t -> unit)
  (* [NewAgent script_run] where [script_run] may produce [Act] effects  *)
  | RunningAgent of (unit, Actions.action) Effect.Deep.continuation
  (* [RunningAgent cont] where [cont] may produce [Act] effects *)

and t = {
  name : string ;
  state : state ref ;
  pos : position ref ;
  speed : float ref ;
  (* speed in actions per second, should be at most 1.0 *)
  action_meter : float ref ;
  (* percentage of time spent waiting for next action *)
  texture : Raylib.Texture.t ref
};;

let get_name (agent : t) : string =
  agent.name

let set_pos (agent : t) (pos : position) =
  agent.pos := pos;;

let get_pos (agent : t) : position =
  !(agent.pos);;

let set_texture (agent : t) (texture : Raylib.Texture.t) : unit =
  agent.texture := texture

let get_texture (agent : t) : Raylib.Texture.t =
  !(agent.texture);;

let create (name : string) (script : t -> unit) (pos : position) ?speed (texture : Raylib.Texture.t) : t =
  {
    name ;
    state = ref (NewAgent script) ;
    pos = ref pos ;
    texture = ref texture ;
    speed = ref (Option.value speed ~default:0.5);
    action_meter = ref 0.0
  }

let resume (agent : t) : Actions.action =
  let open Effect.Deep in
  let open Actions in
  agent.action_meter := !(agent.action_meter) +. (Raylib.get_frame_time ()) *. !(agent.speed) *. Config.speed;
  if !(agent.action_meter) > 1.0 then
    begin
    agent.action_meter := !(agent.action_meter) -. (Float.round !(agent.action_meter));
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
    end
  else
    Actions.Wait

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