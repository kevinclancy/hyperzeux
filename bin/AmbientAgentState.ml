open BoardInterface

(* There is a lot of code duplication between AmbientAgent and Agent.
    Maybe we can use a functor. *)

type channel_handler = (t option, board_interface) Channel.t_in_handler

type _ Effect.t += AmbientAction : unit -> unit Effect.t

type 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (board_interface -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  create_handlers : ('s -> ((t option, board_interface) Channel.t_in_handler) list) option ;

  assert_invariants : (board_interface -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  key_left_pressed : (board_interface -> 's -> t option) option ;
  (** [key_left_pressed self private_data] called when the left-arrow key is pressed *)

  key_right_pressed : (board_interface -> 's -> t option) option ;
  (** [key_right_pressed self private_data] called when the right-arrow key is pressed *)

  key_up_pressed : (board_interface -> 's -> t option) option ;
  (** [key_up_pressed self private_data] callback called when the up-arrow key is pressed *)

  key_down_pressed : (board_interface -> 's -> t option) option ;
  (** [key_down_pressed self private_data] callback called when the down-arrow key is pressed *)

  key_left_released : (board_interface -> 's -> t option) option ;
  (** [key_left_released self private_data] called when the left-arrow key is pressed *)

  key_right_released : (board_interface -> 's -> t option) option ;
  (** [key_right_released self private_data] called when the right-arrow key is pressed *)

  key_up_released : (board_interface -> 's -> t option) option ;
  (** [key_up_released self private_data] callback called when the up-arrow key is pressed *)

  key_down_released : (board_interface -> 's -> t option) option ;
  (** [key_down_released self private_data] callback called when the down-arrow key is pressed *)

  key_space_pressed : (board_interface -> 's -> t option) option ;

  key_space_released : (board_interface -> 's -> t option) option
}

and script_state =
  | BeginScript of (board_interface -> unit)
  (** [BeginScript script] Agent will subsequently begin executing [script] *)
  | RunningAgent of (unit, unit) Effect.Deep.continuation
  (** [RunningAgent cont] where [cont] may produce [Act] effects *)
  | Idling
  (** [Idling] Agent is waiting to respond to stimuli *)

and t = {
  name : string ;
  handlers : channel_handler list ;
  script_state : script_state ref ;
  key_left_pressed : board_interface -> t option ;
  key_right_pressed : board_interface -> t option ;
  key_up_pressed : board_interface -> t option ;
  key_down_pressed : board_interface -> t option ;
  key_left_released : board_interface -> t option ;
  key_right_released : board_interface -> t option ;
  key_up_released : board_interface -> t option ;
  key_down_released : board_interface -> t option ;
  key_space_pressed : board_interface -> t option ;
  key_space_released : board_interface -> t option ;
  assert_invariants : board_interface -> unit
}

type blueprint_props = {
  (** Properties shared by all ambient agent state blueprints, regardless of private data type *)

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

let empty_state_functions = {
  script = None ;
  create_handlers = None ;
  assert_invariants = None ;
  key_left_pressed = None ;
  key_up_pressed = None ;
  key_right_pressed = None ;
  key_down_pressed = None ;
  key_left_released = None ;
  key_up_released = None ;
  key_right_released = None ;
  key_down_released = None ;
  key_space_pressed = None ;
  key_space_released = None ;
}

let create (bp : 's blueprint) (priv_data : 's) : t =
  let state_functions = bp.state_functions in
  {
      name = bp.props.name ;
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
          BeginScript(fun board -> f board priv_data)
        | None ->
          Idling
        end;
      key_left_pressed =
        begin match state_functions.key_left_pressed with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_right_pressed =
        begin match state_functions.key_right_pressed with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun board -> None)
        end;
      key_up_pressed =
        begin match state_functions.key_up_pressed with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_down_pressed =
        begin match state_functions.key_down_pressed with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_left_released =
        begin match state_functions.key_left_released with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_right_released =
        begin match state_functions.key_right_released with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_up_released =
        begin match state_functions.key_up_released with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_down_released =
        begin match state_functions.key_down_released with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_space_released =
        begin match state_functions.key_down_released with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      key_space_pressed =
        begin match state_functions.key_down_released with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun _ -> None)
        end;
      assert_invariants =
        begin match state_functions.assert_invariants with
        | Some f ->
          (fun board -> f board priv_data)
        | None ->
          (fun board -> ())
        end;
    }


(** [create blueprint initial_state] Creates a new agent state from [blueprint]
    using [initial_state] as initial state *)

let name (state : t) : string =
  state.name

let resume (state : t) (board : board_interface) : unit =
  let open Effect.Deep in
  match !(state.script_state) with
  | Idling ->
    ()
  | RunningAgent k ->
    continue k ()
  | BeginScript script ->
    match_with
      (script board)
      ()
      { retc = (fun _ -> state.assert_invariants board; state.script_state := Idling; ()) ;
        exnc = raise ;
        effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | AmbientAction () ->
            Some (function (k : (a, _) continuation) ->
              state.assert_invariants board;
              state.script_state := RunningAgent k;
              ()
            )
          | _ ->
            None
      }

(** [resume state prev_result] Resume the [state]'s coroutine, where [prev_result] tells whether
    the previously yielded action suceeded *)

let handle_messages (state : t) (board : board_interface) : t option =
  let handle_channel (handler : channel_handler) : t option =
    let res = ref None in
    while not (Channel.is_empty handler || Option.is_some !res) do
      res := Channel.handle_one handler board;
    done;
    !res
  in
  List.find_map handle_channel state.handlers

let key_left_pressed (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_left_pressed board in
  state.assert_invariants board;
  opt_new_state

let key_up_pressed (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_up_pressed board in
  state.assert_invariants board;
  opt_new_state

let key_right_pressed (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_right_pressed board in
  state.assert_invariants board;
  opt_new_state

let key_down_pressed (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_down_pressed board in
  state.assert_invariants board;
  opt_new_state

let key_space_pressed (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_space_pressed board in
  state.assert_invariants board;
  opt_new_state

let key_left_released (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_left_released board in
  state.assert_invariants board;
  opt_new_state

let key_up_released (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_up_released board in
  state.assert_invariants board;
  opt_new_state

let key_right_released (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_right_released board in
  state.assert_invariants board;
  opt_new_state

let key_down_released (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_space_released board in
  state.assert_invariants board;
  opt_new_state

let key_space_released (state : t) (board : board_interface) : t option =
  let opt_new_state = state.key_space_released board in
  state.assert_invariants board;
  opt_new_state