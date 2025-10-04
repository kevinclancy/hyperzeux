let say (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (DisplaySpeech (lines, finished));
  while not !finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done

let begin_speech (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (BeginSpeech (lines, finished));
  while not !finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done

let end_speech () : unit =
  let open Channels.Speech in
  Channel.send_msg speech EndSpeech

let set_state (s : RegionAgentState.t) : unit =
  raise (RegionAgentState.ChangeState s)
