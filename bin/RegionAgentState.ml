open Common
open BoardInterface

type _ Effect.t += RegionAction : unit -> unit Effect.t

type channel_handler = (t option, board_interface) Channel.t_in_handler

and 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (board_interface -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  create_handlers : ('s -> channel_handler list) option ;

  assert_invariants : (board_interface -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  on_puppet_enter : (board_interface -> PuppetExternal.t -> 's -> t option) option ;
  (** [on_puppet_enter board_intf puppet] is called when [puppet] enters this region *)

  on_puppet_exit : (board_interface -> PuppetExternal.t -> 's -> t option) option ;
  (** [on_puppet_exit board_intf puppet] is called when [puppet] exits this region *)
}

and resume_result =
  | Continue
  (** [Continue] Tells the agent to remain in its current state *)
  | ChangeState of t
  (** [ChangeState s] Tells the agent to change to state [s] *)

and script_state =
  | BeginScript of (board_interface -> t option)
  (** [BeginScript script] Agent will subsequently begin executing [script]
      Returns [Some(s)] to transition to state [s], or [None] to remain in current state while Idling *)
  | RunningAgent of (unit, resume_result) Effect.Deep.continuation
  (** [RunningAgent cont] where [cont] may produce [Act] effects *)
  | Idling
  (** [Idling] Agent is waiting to respond to stimuli *)

and t = {
  name : string ;
  handlers : channel_handler list ;
  mutable script_state : script_state ;
  on_puppet_enter : board_interface -> PuppetExternal.t -> t option ;
  on_puppet_exit : board_interface -> PuppetExternal.t -> t option ;
  assert_invariants : board_interface -> unit
}

type blueprint_props = {
  (** Properties shared by all agent state blueprints, regardless of private data type *)

  agent_set_invariant : StringSet.t ;
  (** A subset of the set of names of agents that should be contained in the region
      while this state is active *)

  name : string
  (** The name of this state *)
}

type 's blueprint = {
  (** Agent state blueprint, where ['s] is the type of the agent state's private data *)

  state_functions : 's state_functions ;
  (** Functions for interacting with agents in this state *)

  props : blueprint_props
  (** Properties shared by all agent state blueprints, regardless of private data type *)
}

exception ChangeState of t
(** ChangeState(s) signals a change to state [s] *)

let empty_state_functions = {
  script = None ;
  create_handlers = None ;
  on_puppet_enter = None ;
  on_puppet_exit = None ;
  assert_invariants = None ;
}

let create (state_bp : 's blueprint)
           (priv_data : 's) : t =

  let state_functions = state_bp.state_functions in
  {
      name = state_bp.props.name ;
      handlers = begin
        match state_functions.create_handlers with
        | Some(f) ->
          f priv_data
        | None ->
          []
      end;
      script_state =
        begin match state_functions.script with
        | Some f ->
          BeginScript(fun board ->
            try (f board priv_data; None) with
            | ChangeState(s) ->
              Some(s)
          )
        | None ->
          Idling
        end;
      on_puppet_enter =
        begin match state_functions.on_puppet_enter with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      on_puppet_exit =
        begin match state_functions.on_puppet_exit with
        | Some f ->
          (fun board puppet -> f board puppet priv_data)
        | None ->
          (fun _ _ -> None)
        end;
      assert_invariants =
        begin match state_functions.assert_invariants with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun board -> ())
        end;
    }

let name (state : t) : string =
  state.name

let resume (state : t) (board : board_interface) (t_delta_seconds : float) : resume_result =
  let open Effect.Deep in
  let open Actions in
  match state.script_state with
  | Idling ->
    Continue
  | RunningAgent k ->
    continue k ()
  | BeginScript script ->
    match script board with
    | Some(s) ->
      state.assert_invariants board;
      ChangeState(s)
    | None ->
      state.assert_invariants board;
      state.script_state <- Idling;
      Continue
    | effect (RegionAction action), k ->
      state.assert_invariants board;
      state.script_state <- RunningAgent k;
      Continue

let handle_messages (state : t) (board : board_interface) : t option =
  let handle_channel (handler : channel_handler) : t option =
    let res = ref None in
    while not (Channel.is_empty handler || Option.is_some !res) do
      res := Channel.handle_one handler board;
    done;
    !res
  in
  List.find_map handle_channel state.handlers

let on_puppet_enter (state : t) (board : board_interface) (puppet : PuppetExternal.t) : t option =
  let opt_new_state = state.on_puppet_enter board puppet in
  state.assert_invariants board;
  opt_new_state

let on_puppet_exit (state : t) (board : board_interface) (puppet : PuppetExternal.t) : t option =
  let opt_new_state = state.on_puppet_enter board puppet in
  state.assert_invariants board;
  opt_new_state