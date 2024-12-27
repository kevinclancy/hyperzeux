
type 's state_functions = {
  (** Functions for controlling an agent, where ['s] is the type of the agent's private data, i.e. its "memory" *)

  script : (Puppet.t -> 's -> unit) option ;
  (** Coroutine to run while in this state, or None to idle *)

  assert_invariants : (Puppet.t -> 's -> unit) option ;
  (** Function to call to assert state invariants. None means there are no assertable invariants. *)

  receive_bump : (Puppet.t -> 's -> PuppetExternal.t -> t option) option ;
  (** [receive_bump self private_data other] is called when [other] bumps into this agent *)

  key_left : (Puppet.t -> 's -> t option) option ;
  (** [key_left self private_data] callback called when the left-arrow key is pressed *)

  key_right : (Puppet.t -> 's -> t option) option ;
  (** [key_right self private_data] callback called when the right-arrow key is pressed *)

  key_up : (Puppet.t -> 's -> t option) option ;
  (** [key_up self private_data] callback called when the up-arrow key is pressed *)

  key_down : (Puppet.t -> 's -> t option) option ;
  (** [key_down self private_data] callback called when the down-arrow key is pressed *)
}

and script_state =
  | BeginScript of (unit -> unit)
  (** [BeginScript script] Agent will subsequently begin executing [script] *)
  | RunningAgent of (Actions.action_result, Actions.action) Effect.Deep.continuation
  (** [RunningAgent cont] where [cont] may produce [Act] effects *)
  | Idling
  (** [Idling] Agent is waiting to respond to stimuli *)

and t = {
  name : string ;
  region_name : string ;
  script_state : script_state ref ;
  receive_bump : PuppetExternal.t -> t option ;
  key_left : unit -> t option ;
  key_right : unit -> t option ;
  key_up : unit -> t option ;
  key_down : unit -> t option ;
  assert_invariants : unit -> unit
}

module type AgentStateClass = sig
  (** Definition of a state of an agent. *)

  type t_private_data
  (** Type of private data of an instance of this agent state *)

  val state_functions : unit -> t_private_data state_functions
  (** Functions for interacting with agents in this state, where 't is the agent type *)

  val region_name : unit -> string
  (** Name of region that the agent is expected to remain within while in this state *)

  val name : unit -> string
  (** The name of this state *)
end

let empty_state_functions = {
  script = None ;
  receive_bump = None ;
  assert_invariants = None ;
  key_left = None ;
  key_up = None ;
  key_right = None ;
  key_down = None
}

let create (type s)
           (state_class : (module AgentStateClass with type t_private_data = s))
           (priv_data : s)
           (puppet : Puppet.t) : t =

  let module C = (val state_class : (AgentStateClass with type t_private_data = s)) in
  let state_functions = C.state_functions () in
  {
      name = C.name ();
      region_name = C.region_name ();
      script_state =
        ref begin match state_functions.script with
        | Some f ->
          BeginScript(fun () -> f puppet priv_data)
        | None ->
          Idling
        end;
      receive_bump =
        begin match state_functions.receive_bump with
        | Some f ->
          (fun puppet_ext -> f puppet priv_data puppet_ext)
        | None ->
          (fun _ -> None)
        end;
      key_left =
        begin match state_functions.key_left with
        | Some f ->
          (fun () -> f puppet priv_data)
        | None ->
          (fun () -> None)
        end;
      key_right =
        begin match state_functions.key_right with
        | Some f ->
          (fun () -> f puppet priv_data)
        | None ->
          (fun () -> None)
        end;
      key_up =
        begin match state_functions.key_up with
        | Some f ->
          (fun () -> f puppet priv_data)
        | None ->
          (fun () -> None)
        end;
      key_down =
        begin match state_functions.key_down with
        | Some f ->
          (fun () -> f puppet priv_data)
        | None ->
          (fun () -> None)
        end;
      assert_invariants =
        begin match state_functions.assert_invariants with
        | Some f ->
          (fun () -> f puppet priv_data)
        | None ->
          (fun () -> ())
        end;
    }

let name (state : t) : string =
  state.name

let state_region_name (state : t) : string =
  state.region_name

let resume (state : t) (prev_result : Actions.action_result) : Actions.action =
  let open Effect.Deep in
  let open Actions in
  match !(state.script_state) with
  | Idling ->
    Actions.Idle
  | RunningAgent k ->
    continue k prev_result
  | BeginScript script ->
    match_with
      script
      ()
      { retc = (fun _ -> state.assert_invariants (); state.script_state := Idling; Actions.Wait) ;
        exnc = raise ;
        effc = fun (type a) (eff : a Effect.t) ->
          match eff with
          | Act action ->
            Some (function (k : (a, _) continuation) ->
              state.assert_invariants ();
              state.script_state := RunningAgent k;
              action
            )
          | _ ->
            None
      }

let receive_bump (state : t) (other : PuppetExternal.t) : t option =
  let opt_new_state = state.receive_bump other in
  state.assert_invariants ();
  opt_new_state

let key_left (state : t) : t option =
  let opt_new_state = state.key_left () in
  state.assert_invariants ();
  opt_new_state

let key_up (state : t) : t option =
  let opt_new_state = state.key_up () in
  state.assert_invariants ();
  opt_new_state

let key_right (state : t) : t option =
  let opt_new_state = state.key_right () in
  state.assert_invariants ();
  opt_new_state

let key_down (state : t) : t option =
  let opt_new_state = state.key_down () in
  state.assert_invariants ();
  opt_new_state