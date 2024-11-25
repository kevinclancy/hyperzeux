open Common

type state =
  | BeginScript of (t -> unit)
  (** [BeginScript script] Agent will subsequently begin executing [script] *)
  | RunningAgent of (Actions.action_result, Actions.action) Effect.Deep.continuation
  (** [RunningAgent cont] where [cont] may produce [Act] effects *)
  | Idling
  (** [Idling] Agent is waiting to respond to stimuli *)

and event_handlers = {
    initial : (t -> unit) option ;
    (** Script to begin running when we enter the state, or begin idling if None *)

    receive_bump : (t -> t -> unit) option ;
    (** [receive_bump self other] is called when [other] bumps into this agent *)

    assert_invariants : (t -> unit) option ;

    key_left : (t -> unit) option ;
    key_right : (t -> unit) option ;
    key_up : (t -> unit) option ;
    key_down : (t -> unit) option
}

and t = {
  name : string ;
  state : state ref ;
  pos : position ref ;

  event_handlers : event_handlers ref ;

  speed : float ref ;
  (** speed in actions per second, should be at most 1.0 *)

  action_meter : float ref ;
  (** percentage of wait completed before next action is allowed *)

  texture : Raylib.Texture.t ref ;
  (** current texture depicting the agent *)

  color : Raylib.Color.t ref
  (** current color depicting agent *)
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

let receive_bump (self : t) (other : t) : unit =
  match !(self.event_handlers).receive_bump with
  | Some(handler) ->
    handler self other
  | None ->
    ()

let empty_scripts = {
  initial = None ;
  receive_bump = None ;
  assert_invariants = None ;
  key_left = None ;
  key_up = None ;
  key_right = None ;
  key_down = None
}

let create (name : string) (event_handlers : event_handlers) (pos : position) (color : Raylib.Color.t) ?speed (texture : Raylib.Texture.t) : t =
  {
    name ;
    state =
      (match event_handlers.initial with
       | Some(script) ->
         ref (BeginScript script)
       | None ->
         ref (Idling)) ;
    event_handlers = ref event_handlers;
    pos = ref pos ;
    color = ref color;
    texture = ref texture ;
    speed = ref (Option.value speed ~default:0.5);
    action_meter = ref 0.0
  }

let rec resume (agent : t) (prev_result : Actions.action_result): Actions.action =
  let open Effect.Deep in
  let open Actions in
  agent.action_meter := !(agent.action_meter) +. (Raylib.get_frame_time ()) *. !(agent.speed) *. Config.speed;
  if !(agent.action_meter) > 1.0 then
    begin
    agent.action_meter := !(agent.action_meter) -. (Float.round !(agent.action_meter));
    match !(agent.state) with
    | Idling ->
      Actions.Idle
    | RunningAgent k ->
      continue k prev_result
    | BeginScript script ->
      match_with
        script
        agent
        { retc = (fun _ -> agent.state := Idling; Actions.Wait) ;
          exnc = raise ;
          effc = fun (type a) (eff : a Effect.t) ->
            match eff with
            | Act action ->
              Some (function (k : (a, _) continuation) ->
                Option.iter (fun f -> f agent) !(agent.event_handlers).assert_invariants;
                agent.state := RunningAgent k;
                action
              )
            | _ ->
              None }
    end
  else
    Actions.Idle

let update_input (agent : t) : unit =
  let open Raylib in
  if is_key_pressed Key.Left && Option.is_some !(agent.event_handlers).key_left then
    begin
      let script (agent : t) =
        (Option.get !(agent.event_handlers).key_left) agent;
        while is_key_down Key.Left do
          (Option.get !(agent.event_handlers).key_left) agent
        done
      in
      agent.state := BeginScript script
    end
  else if is_key_pressed Key.Right && Option.is_some !(agent.event_handlers).key_right then
    begin
      let script (agent : t) =
        (Option.get !(agent.event_handlers).key_right) agent;
        while is_key_down Key.Right do
          (Option.get !(agent.event_handlers).key_right) agent
        done
      in
      agent.state := BeginScript script
    end
  else if is_key_pressed Key.Up && Option.is_some !(agent.event_handlers).key_up then
    begin
      let script (agent : t) =
        (Option.get !(agent.event_handlers).key_up) agent;
        while is_key_down Key.Up do
          (Option.get !(agent.event_handlers).key_up) agent
        done
      in
      agent.state := BeginScript script
    end
  else if is_key_pressed Key.Down && Option.is_some !(agent.event_handlers).key_down then
    begin
      let script (agent : t) =
        (Option.get !(agent.event_handlers).key_down) agent;
        while is_key_down Key.Down do
          (Option.get !(agent.event_handlers).key_down) agent
        done
      in
      agent.state := BeginScript script
    end