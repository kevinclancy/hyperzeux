open BoardInterface

type channel_handler = (t option, board_interface * Puppet.t) Channel.t_in_handler

and 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (board_interface -> Puppet.t -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  create_handlers : ('s -> channel_handler list) option ;

  assert_invariants : (board_interface -> Puppet.t -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  receive_bump : (board_interface -> Puppet.t -> 's -> PuppetExternal.t -> t option) option ;
  (** [receive_bump self private_data other] is called when [other] bumps into this agent *)

  key_left_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_left_pressed self private_data] called when the left-arrow key is pressed *)

  key_right_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_right_pressed self private_data] called when the right-arrow key is pressed *)

  key_up_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_up_pressed self private_data] callback called when the up-arrow key is pressed *)

  key_down_pressed : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_down_pressed self private_data] callback called when the down-arrow key is pressed *)

  key_left_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_left_released self private_data] called when the left-arrow key is pressed *)

  key_right_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_right_released self private_data] called when the right-arrow key is pressed *)

  key_up_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_up_released self private_data] callback called when the up-arrow key is pressed *)

  key_down_released : (board_interface -> Puppet.t -> 's -> t option) option ;
  (** [key_down_released self private_data] callback called when the down-arrow key is pressed *)
}

and script_state =
  | BeginScript of (board_interface -> Puppet.t -> unit)
  (** [BeginScript script] Agent will subsequently begin executing [script] *)
  | RunningAgent of (Actions.action_result, Actions.action) Effect.Deep.continuation
  (** [RunningAgent cont] where [cont] may produce [Act] effects *)
  | Idling
  (** [Idling] Agent is waiting to respond to stimuli *)

and t = {
  name : string ;
  region_name : string option ;
  handlers : channel_handler list ;
  script_state : script_state ref ;
  receive_bump : board_interface -> Puppet.t -> PuppetExternal.t -> t option ;
  key_left_pressed : board_interface -> Puppet.t -> t option ;
  key_right_pressed : board_interface -> Puppet.t -> t option ;
  key_up_pressed : board_interface -> Puppet.t -> t option ;
  key_down_pressed : board_interface -> Puppet.t -> t option ;
  key_left_released : board_interface -> Puppet.t -> t option ;
  key_right_released : board_interface -> Puppet.t -> t option ;
  key_up_released : board_interface -> Puppet.t -> t option ;
  key_down_released : board_interface -> Puppet.t -> t option ;
  assert_invariants : board_interface -> Puppet.t -> unit
}

module type AgentStateClass = sig
  (** Definition of a state of an agent. *)

  type t_private_data
  (** Type of private data of an instance of this agent state *)

  val state_functions : unit -> t_private_data state_functions
  (** Functions for interacting with agents in this state, where 't is the agent type *)

  val region_name : unit -> string option
  (** Name of region that the agent is expected to remain within while in this state,
      or None if no such expectation exists *)

  val name : unit -> string
  (** The name of this state *)
end

let empty_state_functions = {
  script = None ;
  create_handlers = None ;
  receive_bump = None ;
  assert_invariants = None ;
  key_left_pressed = None ;
  key_up_pressed = None ;
  key_right_pressed = None ;
  key_down_pressed = None ;
  key_left_released = None ;
  key_up_released = None ;
  key_right_released = None ;
  key_down_released = None
}

let create (type s)
           (state_class : (module AgentStateClass with type t_private_data = s))
           (priv_data : s) : t =

  let module C = (val state_class : (AgentStateClass with type t_private_data = s)) in
  let in_region (board : board_interface) (puppet : Puppet.t) : bool =
    match C.region_name () with
    | Some(region_name) ->
      let r = board.get_region region_name in
      Region.contains r (Puppet.get_pos puppet)
    | None ->
      true
  in
  let state_functions = C.state_functions () in
  {
      name = C.name ();
      region_name = C.region_name ();
      handlers = begin
        match state_functions.create_handlers with
        | Some(f) ->
          f priv_data
        | None ->
          []
      end;
      script_state =
        ref begin match state_functions.script with
        | Some f ->
          BeginScript(fun board puppet -> f board puppet priv_data)
        | None ->
          Idling
        end;
      receive_bump =
        begin match state_functions.receive_bump with
        | Some f ->
          (fun board puppet puppet_ext -> f board puppet priv_data puppet_ext)
        | None ->
          (fun _ _ _ -> None)
        end;
      key_left_pressed =
        begin match state_functions.key_left_pressed with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      key_right_pressed =
        begin match state_functions.key_right_pressed with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun board _ -> None)
        end;
      key_up_pressed =
        begin match state_functions.key_up_pressed with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      key_down_pressed =
        begin match state_functions.key_down_pressed with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      key_left_released =
        begin match state_functions.key_left_released with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      key_right_released =
        begin match state_functions.key_right_released with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      key_up_released =
        begin match state_functions.key_up_released with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      key_down_released =
        begin match state_functions.key_down_released with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      assert_invariants =
        begin match state_functions.assert_invariants with
        | Some f ->
          (fun board puppet -> f board puppet priv_data; assert (in_region board puppet))
        | None ->
          (fun board puppet -> assert (in_region board puppet))
        end;
    }

let name (state : t) : string =
  state.name

let region_name (state : t) : string option =
  state.region_name

let resume (state : t) (board : board_interface) (puppet : Puppet.t) (prev_result : Actions.action_result) : Actions.action =
  let open Effect.Deep in
  let open Actions in
  match !(state.script_state) with
  | Idling ->
    Actions.Idle
  | RunningAgent k ->
    continue k prev_result
  | BeginScript script ->
    match_with
      (script board)
      puppet
      { retc = (fun _ -> state.assert_invariants board puppet; state.script_state := Idling; Actions.Wait) ;
        exnc = raise ;
        effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Act action ->
            Some (function (k : (a, _) continuation) ->
              state.assert_invariants board puppet;
              state.script_state := RunningAgent k;
              action
            )
          | _ ->
            None
      }

let handle_messages (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let handle_channel (handler : channel_handler) : t option =
    let res = ref None in
    while not (Channel.is_empty handler || Option.is_some !res) do
      res := Channel.handle_one handler (board, puppet);
    done;
    !res
  in
  List.find_map handle_channel state.handlers

let receive_bump (state : t) (board : board_interface) (puppet : Puppet.t) (other : PuppetExternal.t) : t option =
  let opt_new_state = state.receive_bump board puppet other in
  state.assert_invariants board puppet;
  opt_new_state

let key_left_pressed (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_left_pressed board puppet in
  state.assert_invariants board puppet;
  opt_new_state

let key_up_pressed (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_up_pressed board puppet in
  state.assert_invariants board puppet;
  opt_new_state

let key_right_pressed (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_right_pressed board puppet in
  state.assert_invariants board puppet;
  opt_new_state

let key_down_pressed (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_down_pressed board puppet in
  state.assert_invariants board puppet;
  opt_new_state

let key_left_released (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_left_released board puppet in
  state.assert_invariants board puppet;
  opt_new_state

let key_up_released (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_up_released board puppet in
  state.assert_invariants board puppet;
  opt_new_state

let key_right_released (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_right_released board puppet in
  state.assert_invariants board puppet;
  opt_new_state

let key_down_released (state : t) (board : board_interface) (puppet : Puppet.t) : t option =
  let opt_new_state = state.key_down_released board puppet in
  state.assert_invariants board puppet;
  opt_new_state