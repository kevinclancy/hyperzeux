open Common
open AgentState

type board_interface = {
  get_waypoint : string -> position
  (** Retrieve the waypoint with the given name *)
}

module type AgentClass = sig
  val states : (module AgentStateClass) StringMap.t
  (** Maps name of each state that agents of this class can enter to the state itself *)

  val initial_state : (module AgentStateClass with type t_private_data = unit)
  (** The state that the agent starts out in *)

  val preview_texture_name : string
  (** The name of the texture used to represent the agent class in the map editor *)

  val preview_color : Raylib.Color.t
  (** The color that the preview texture is drawn in the map editor *)

  val speed : float

  val name : string
  (** The name of the agent class *)
end

type t = {
  agent_class : (module AgentClass) ;

  puppet : Puppet.t ;
  (** The physical body this agent controls *)

  agent_state : AgentState.t ref ;
  (** The current scripts used to control the puppet *)

  speed : float ref ;
  (** speed in actions per second, should be at most 1.0 *)

  action_meter : float ref ;
  (** percentage of wait completed before next action is allowed *)
}

let create (board : board_interface)
           (agent_class : (module AgentClass))
           (initial_state_class : (module AgentStateClass with type t_private_data = unit))
           (name : string)
           (position : position)
           (color : Raylib.Color.t)
           : t =

  let module C = (val agent_class : AgentClass) in
  let module S = (val initial_state_class : AgentStateClass with type t_private_data = unit) in
  let puppet = Puppet.create name position color (TextureMap.get C.preview_texture_name) in
  let initial_state = AgentState.create (module S) () puppet in
  {
    agent_class ;
    agent_state = ref initial_state ;
    puppet ;
    action_meter = ref 0.;
    speed  = ref C.speed;
  }

(** [create board_intf name pos color] Creates an agent named [name] at position [pos] with color [color] *)

let name (agent : t) : string =
  Puppet.get_name agent.puppet

let agent_class (agent : t) =
  agent.agent_class

let update_input (agent : t) : unit =
  let open Raylib in
  let opt_new_state =
    if is_key_pressed Key.Left then
      AgentState.key_left !(agent.agent_state)
    else if is_key_pressed Key.Up then
      AgentState.key_up !(agent.agent_state)
    else if is_key_pressed Key.Right then
      AgentState.key_right !(agent.agent_state)
    else if is_key_pressed Key.Down then
      AgentState.key_down !(agent.agent_state)
    else
      None
  in
  Option.iter (fun state -> agent.agent_state := state) opt_new_state

let receive_bump (agent : t) (other : PuppetExternal.t) : unit =
  let opt_new_state = AgentState.receive_bump !(agent.agent_state) other in
  Option.iter (fun state -> agent.agent_state := state) opt_new_state

let rec resume (agent : t) (prev_result : Actions.action_result) : Actions.action =
  let open Effect.Deep in
  let open Actions in
  agent.action_meter := !(agent.action_meter) +. (Raylib.get_frame_time ()) *. !(agent.speed) *. Config.speed;
  if !(agent.action_meter) > 1.0 then
    begin
      agent.action_meter := !(agent.action_meter) -. (Float.round !(agent.action_meter));
      AgentState.resume !(agent.agent_state) prev_result
    end
  else
    Actions.Idle

let position (agent : t) : position =
  Puppet.get_pos agent.puppet

let color (agent : t) : Raylib.Color.t =
  Puppet.get_color agent.puppet

let set_position (agent : t) (pos : position) : unit =
  Puppet.set_pos agent.puppet pos

let texture (agent : t) : Raylib.Texture.t =
  Puppet.get_texture agent.puppet