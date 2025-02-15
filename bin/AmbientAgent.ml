open Common
open AmbientAgentState
open BoardInterface

type ambient_agent_class = {
  states : AmbientAgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : AmbientAgentState.t ;
  (** The state that the agent starts out in *)

  preview_texture_name : string ;
  (** The name of the texture used to represent the agent class in the map editor *)

  speed : float ;

  name : string ;
  (** The name of the agent class *)
}

type t = {
  name : string ;
  (** The name of the agent *)

  board : board_interface ;
  (** Interface to the board this agent part of *)

  mutable agent_state : AmbientAgentState.t ;
  (** The current scripts and set of callbacks used by this agent *)

  speed : float ref ;
  (** speed in actions per second, should be at most 1.0 *)

  action_meter : float ref ;
  (** percentage of wait completed before next action is allowed *)
}

and script_state =
  | BeginScript of (board_interface -> unit)
  (** [BeginScript script] Agent will subsequently begin executing [script] *)
  | RunningAgent of (unit, unit) Effect.Deep.continuation
  (** [RunningAgent cont] where [cont] may produce [Act] effects *)
  | Idling
  (** [Idling] Agent is waiting to respond to stimuli *)

let create (board : board_interface)
           (agent_class : ambient_agent_class)
           (name : string)
           : t =
  {
    name ;
    board ;
    agent_state = agent_class.initial_state ;
    action_meter = ref 0.;
    speed  = ref agent_class.speed;
  }

let name (state : t) : string =
  state.name

let update_input (agent : t) : unit =
  let open Raylib in
  let opt_new_state =
    if is_key_pressed Key.Left then
      AmbientAgentState.key_left_pressed agent.agent_state agent.board
    else if is_key_pressed Key.Up then
      AmbientAgentState.key_up_pressed agent.agent_state agent.board
    else if is_key_pressed Key.Right then
      AmbientAgentState.key_right_pressed agent.agent_state agent.board
    else if is_key_pressed Key.Down then
      AmbientAgentState.key_down_pressed agent.agent_state agent.board
    else if is_key_released Key.Left then
      AmbientAgentState.key_left_released agent.agent_state agent.board
    else if is_key_released Key.Up then
      AmbientAgentState.key_up_released agent.agent_state agent.board
    else if is_key_released Key.Right then
      AmbientAgentState.key_right_released agent.agent_state agent.board
    else if is_key_released Key.Down then
      AmbientAgentState.key_down_released agent.agent_state agent.board
    else
      None
  in
  Option.iter (fun state -> agent.agent_state <- state) opt_new_state

let rec resume (agent : t) : unit =
  let open Effect.Deep in
  let open Actions in
  agent.action_meter := !(agent.action_meter) +. (Raylib.get_frame_time ()) *. !(agent.speed) *. Config.speed;
  if !(agent.action_meter) > 1.0 then
    begin
      agent.action_meter := !(agent.action_meter) -. (Float.round !(agent.action_meter));
      AmbientAgentState.resume agent.agent_state agent.board
    end
  else
    ()

let handle_messages (agent : t) : unit =
  match AmbientAgentState.handle_messages agent.agent_state agent.board with
  | Some(new_state) ->
    agent.agent_state <- new_state
  | None ->
    ()