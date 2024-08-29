
type state =
  (* [NewAgent script_run] where [script_run] may produce [Act] effects  *)
  | NewAgent of (t -> unit)
  (* [RunningAgent cont] where [cont] may produce [Act] effects *)
  | RunningAgent of (unit, Actions.action) Effect.Deep.continuation

and t = {
  state : state ref ;
  pos : (int * int) ref ;
  texture : Raylib.Texture.t ref
};;

let set_pos (agent : t) (pos : int * int) =
  agent.pos := pos;;

let get_pos (agent : t) : int * int =
  !(agent.pos);;

let set_texture (agent : t) (texture : Raylib.Texture.t) : unit =
  agent.texture := texture

let get_texture (agent : t) : Raylib.Texture.t =
  !(agent.texture);;

let create (script : t -> unit) (pos : int * int) (texture : Raylib.Texture.t) : t =
  { state = ref (NewAgent script) ; pos = ref pos ; texture = ref texture }

let resume (agent : t) : Actions.action =
  let open Effect.Deep in
  let open Actions in
  let f () =
    match !(agent.state) with
    | RunningAgent k ->
      continue k ()
    | NewAgent run ->
      run agent;
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
          Some (function (k : (a, _) continuation) -> agent.state := RunningAgent k; action)
        | _ ->
          None
    }