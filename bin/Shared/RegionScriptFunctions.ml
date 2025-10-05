let say (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (DisplaySpeech (lines, None, finished));
  while not !finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done

let begin_speech (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (BeginSpeech (lines, None, finished));
  while not !finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done

let end_speech () : unit =
  let open Channels.Speech in
  Channel.send_msg speech EndSpeech

let acquire (agent_name : string) : AgentStateCreators.AcquiredState.command_channel =
  let acquire_channel = AgentStateCreators.AcquiredState.get_acquire_channel agent_name in
  let ref_opt_command_channel = ref None in
  Channel.send_msg acquire_channel ref_opt_command_channel;
  while Option.is_none !ref_opt_command_channel do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  Option.get !ref_opt_command_channel

let send_command
  (command_channel : AgentStateCreators.AcquiredState.command_channel)
  (script_fn : AgentStateCreators.AcquiredState.script_fn)
  : bool ref =

  (** [send_command command_channel script_fn] sends a command along a command channel,
      and returns a reference that becomes true when the command has finished executing *)

  let is_finished = ref false in
  Channel.send_msg command_channel (script_fn, is_finished);
  is_finished

let perform_command
  (command_channel : AgentStateCreators.AcquiredState.command_channel)
  (script_fn : AgentStateCreators.AcquiredState.script_fn)
  : unit =

  (** [perform_command command_channel script_fn] sends a command along a command channel, and returns once the
      command has finished executing. *)

  let is_finished = send_command command_channel script_fn in
  while not !is_finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  ()


let set_state (s : RegionAgentState.t) : unit =
  raise (RegionAgentState.ChangeState s)
