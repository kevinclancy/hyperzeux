open Common

type state =
  | BeginScript of (t -> unit)
  (** [BeginScript script] Agent will subsequently begin executing [script] *)
  | RunningAgent of (unit, Actions.action) Effect.Deep.continuation
  (** [RunningAgent cont] where [cont] may produce [Act] effects *)
  | Idling
  (** [Idling] Agent is waiting to respond to stimuli *)

and scripts = {
    initial : (t -> unit) option ;
    key_left : (t -> unit) option ;
    key_right : (t -> unit) option ;
    key_up : (t -> unit) option ;
    key_down : (t -> unit) option
}

and t = {
  name : string ;
  state : state ref ;
  pos : position ref ;

  scripts : scripts ;

  speed : float ref ;
  (** speed in actions per second, should be at most 1.0 *)

  action_meter : float ref ;
  (** percentage of wait completed before next action is allowed *)

  texture : Raylib.Texture.t ref ;
  (** percentage of time spent waiting for next action *)

  color : Raylib.Color.t ref
  (** color to display agent in *)
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

let get_color (agent : t) : Raylib.Color.t =
  !(agent.color)

let empty_scripts = {
  initial = None ;
  key_left = None ;
  key_up = None ;
  key_right = None ;
  key_down = None
}

let create (name : string) (scripts : scripts) (pos : position) (color : Raylib.Color.t) ?speed (texture : Raylib.Texture.t) : t =
  {
    name ;
    state =
      (match scripts.initial with
       | Some(script) ->
         ref (BeginScript script)
       | None ->
         ref (Idling)) ;
    scripts ;
    pos = ref pos ;
    color = ref color;
    texture = ref texture ;
    speed = ref (Option.value speed ~default:0.5);
    action_meter = ref 0.0
  }

let rec resume (agent : t) : Actions.action =
  let open Effect.Deep in
  let open Actions in
  agent.action_meter := !(agent.action_meter) +. (Raylib.get_frame_time ()) *. !(agent.speed) *. Config.speed;
  if !(agent.action_meter) > 1.0 then
    begin
    agent.action_meter := !(agent.action_meter) -. (Float.round !(agent.action_meter));
    match !(agent.state) with
    | Idling ->
      Actions.Wait
    | RunningAgent k ->
      continue k ()
    | BeginScript script ->
      match_with
        script
        agent
        { retc = (fun _ -> agent.state := Idling; Actions.Wait) ;
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

let update_input (agent : t) : unit =
  let open Raylib in
  if is_key_pressed Key.Left && Option.is_some agent.scripts.key_left then
    begin
      let script (agent : t) =
        (Option.get agent.scripts.key_left) agent;
        while is_key_down Key.Left do
          (Option.get agent.scripts.key_left) agent
        done
      in
      agent.state := BeginScript script
    end
  else if is_key_pressed Key.Right && Option.is_some agent.scripts.key_right then
    begin
      let script (agent : t) =
        (Option.get agent.scripts.key_right) agent;
        while is_key_down Key.Right do
          (Option.get agent.scripts.key_right) agent
        done
      in
      agent.state := BeginScript script
    end
  else if is_key_pressed Key.Up && Option.is_some agent.scripts.key_up then
    begin
      let script (agent : t) =
        (Option.get agent.scripts.key_up) agent;
        while is_key_down Key.Up do
          (Option.get agent.scripts.key_up) agent
        done
      in
      agent.state := BeginScript script
    end
  else if is_key_pressed Key.Down && Option.is_some agent.scripts.key_down then
    begin
      let script (agent : t) =
        (Option.get agent.scripts.key_down) agent;
        while is_key_down Key.Down do
          (Option.get agent.scripts.key_down) agent
        done
      in
      agent.state := BeginScript script
    end

(* let opt_run_key_left (agent : t) : unit =
  if Option.is_some agent.scripts.key_left then
    begin
      let script (agent : t) =
        while true do
          (Option.get agent.scripts.key_left) agent
        done
      in
      agent.state := BeginScript script
    end

let opt_run_key_right (agent : t) : unit =
  if Option.is_some agent.scripts.key_right then
    begin
      let script (agent : t) =
        while true do
          (Option.get agent.scripts.key_right) agent
        done
      in
      agent.state := BeginScript script
    end

let idle (agent : t) =
  agent.state := Idling *)