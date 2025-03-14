open Common
open AmbientAgentState
open BoardInterface

type camera_agent_class = {
  states : CameraAgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : CameraAgentState.t ;
  (** The state that the agent starts out in *)

  speed : float ;

  name : string ;
  (** The name of the agent class *)
}

type t = {
  name : string ;
  (** The name of the agent *)

  board : board_interface ;
  (** Interface to the board this agent part of *)

  mutable agent_state : CameraAgentState.t ;
  (** The current scripts and set of callbacks used by this agent *)

  speed : float ref ;
  (** speed in actions per second, should be at most 1.0 *)

  action_meter : float ref ;
  (** percentage of wait completed before next action is allowed *)
}

let create (board : board_interface)
           (agent_class : camera_agent_class)
           : t =
  {
    name = agent_class.name;
    board ;
    agent_state = agent_class.initial_state ;
    action_meter = ref 0.;
    speed  = ref agent_class.speed;
  }

let name (agent : t) : string =
  agent.name

let update_input (agent : t) : unit =
  let open Raylib in
  let opt_new_state =
    if is_key_pressed Key.Left then
      CameraAgentState.key_left_pressed agent.agent_state agent.board
    else if is_key_pressed Key.Up then
      CameraAgentState.key_up_pressed agent.agent_state agent.board
    else if is_key_pressed Key.Right then
      CameraAgentState.key_right_pressed agent.agent_state agent.board
    else if is_key_pressed Key.Down then
      CameraAgentState.key_down_pressed agent.agent_state agent.board
    else if is_key_pressed Key.Space then
      CameraAgentState.key_space_pressed agent.agent_state agent.board
    else if is_key_released Key.Left then
      CameraAgentState.key_left_released agent.agent_state agent.board
    else if is_key_released Key.Up then
      CameraAgentState.key_up_released agent.agent_state agent.board
    else if is_key_released Key.Right then
      CameraAgentState.key_right_released agent.agent_state agent.board
    else if is_key_released Key.Down then
      CameraAgentState.key_down_released agent.agent_state agent.board
    else if is_key_released Key.Space then
      CameraAgentState.key_space_released agent.agent_state agent.board
    else
      None
  in
  Option.iter (fun state -> agent.agent_state <- state) opt_new_state

let rec resume (agent : t) (t_delta_seconds : float) : unit =
  let open Effect.Deep in
  let open Actions in
  agent.action_meter := !(agent.action_meter) +. (Raylib.get_frame_time ()) *. !(agent.speed) *. Config.speed;
  if !(agent.action_meter) > 1.0 then
    begin
      agent.action_meter := !(agent.action_meter) -. (Float.round !(agent.action_meter));
      CameraAgentState.resume agent.agent_state agent.board t_delta_seconds
    end
  else
    ()

let get_pos (agent : t) : vec2 =
  CameraAgentState.get_pos agent.agent_state

let handle_messages (agent : t) : unit =
  match CameraAgentState.handle_messages agent.agent_state agent.board with
  | Some(new_state) ->
    agent.agent_state <- new_state
  | None ->
    ()