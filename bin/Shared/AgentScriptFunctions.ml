let say (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (DisplaySpeech (lines, finished));
  while not !finished do
    ignore (Effect.perform @@ Actions.Act Actions.Wait)
  done

let begin_speech (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (BeginSpeech (lines, finished));
  while not !finished do
    ignore (Effect.perform @@ Actions.Act Actions.Wait)
  done

let end_speech () : unit =
  let open Channels.Speech in
  Channel.send_msg speech EndSpeech

type walk_functions = {
  walk_north : Puppet.t -> unit ;
  (** Walk the puppet one cell north *)
  walk_east : Puppet.t -> unit ;
  (** Walk the puppet one cell east *)
  walk_south : Puppet.t -> unit ;
  (** Walk the puppet one cell south *)
  walk_west : Puppet.t -> unit ;
  (** Walk the puppet one cell west *)
}

let walk_to (board : BoardInterface.board_interface) (waypoint_name : string) (walk_fns : walk_functions) (puppet : Puppet.t) : unit =
  (** [walk_to board waypoint_name walk_fns puppet] walks [puppet] to the waypoint named [waypoint_name]
      using the walk functions in [walk_fns]. *)
  let waypoint_pos = board.get_waypoint waypoint_name in
  let rec attempt_walk () =
    let puppet_pos = Puppet.get_pos puppet in
    match board.get_path puppet_pos waypoint_pos with
    | None ->
      Actions.wait 5;
      attempt_walk ()
    | Some path ->
      List.iter (fun dir ->
        match dir with
        | Common.North -> walk_fns.walk_north puppet
        | Common.East -> walk_fns.walk_east puppet
        | Common.South -> walk_fns.walk_south puppet
        | Common.West -> walk_fns.walk_west puppet
      ) path
  in
  attempt_walk ()

let set_state (s : AgentState.t) : unit =
  raise (AgentState.ChangeState s)