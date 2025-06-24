open Common
open RegionAgentState
open BoardInterface

type region_agent_class = {
  states : RegionAgentState.blueprint_props StringMap.t ;
  (** Maps name of each state that agents of this class can enter to the state itself *)

  initial_state : RegionAgentState.t ;
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

  mutable agent_state : RegionAgentState.t ;
  (** The current scripts and set of callbacks used by this agent *)

  speed : float ref ;
  (** speed in actions per second, should be at most 1.0 *)

  action_meter : float ref ;
  (** percentage of wait completed before next action is allowed *)
}

let create (board : board_interface)
           (agent_class : region_agent_class)
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

let on_puppet_enter (agent : t) (board : board_interface) (puppet : PuppetExternal.t) : unit =
  match RegionAgentState.on_puppet_enter agent.agent_state board puppet with
  | Some(next_state) ->
    agent.agent_state <- next_state
  | None ->
    ()

let on_puppet_exit (agent : t) (board : board_interface) (puppet : PuppetExternal.t) : unit =
  match RegionAgentState.on_puppet_exit agent.agent_state board puppet with
  | Some(next_state) ->
    agent.agent_state <- next_state
  | None ->
    ()

let rec resume (agent : t) : unit =
  let open Effect.Deep in
  agent.action_meter := !(agent.action_meter) +. (Raylib.get_frame_time ()) *. !(agent.speed) *. Config.speed;
  if !(agent.action_meter) > 1.0 then
    begin
      agent.action_meter := !(agent.action_meter) -. (Float.round !(agent.action_meter));
      match RegionAgentState.resume agent.agent_state agent.board with
      | Continue ->
        ()
      | ChangeState(next_state) ->
        agent.agent_state <- next_state
    end
  else
    ()

let handle_messages (agent : t) : unit =
  match RegionAgentState.handle_messages agent.agent_state agent.board with
  | Some(new_state) ->
    agent.agent_state <- new_state
  | None ->
    ()