open Common
open AmbientAgentState
open BoardInterface

type ambient_agent_class = {
  states : AmbientAgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : AmbientAgentState.t ;
  (** The state that the agent starts out in *)

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
}

let create (board : board_interface)
           (agent_class : ambient_agent_class)
           : t =
  {
    name = agent_class.name;
    board ;
    agent_state = agent_class.initial_state ;
  }

let draw (agent : t) : unit =
  AmbientAgentState.draw agent.agent_state

let name (agent : t) : string =
  agent.name

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
    else if is_key_pressed Key.Space then
      AmbientAgentState.key_space_pressed agent.agent_state agent.board
    else if is_key_released Key.Left then
      AmbientAgentState.key_left_released agent.agent_state agent.board
    else if is_key_released Key.Up then
      AmbientAgentState.key_up_released agent.agent_state agent.board
    else if is_key_released Key.Right then
      AmbientAgentState.key_right_released agent.agent_state agent.board
    else if is_key_released Key.Down then
      AmbientAgentState.key_down_released agent.agent_state agent.board
    else if is_key_released Key.Space then
      AmbientAgentState.key_space_released agent.agent_state agent.board
    else
      None
  in
  Option.iter (fun state -> agent.agent_state <- state) opt_new_state

let rec resume (agent : t) : unit =
  AmbientAgentState.resume agent.agent_state agent.board

let handle_messages (agent : t) : unit =
  match AmbientAgentState.handle_messages agent.agent_state agent.board with
  | Some(new_state) ->
    agent.agent_state <- new_state
  | None ->
    ()