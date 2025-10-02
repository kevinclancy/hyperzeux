let say (msg : string) : unit =
  let open Channels.Speech in
  let finished = ref false in
  let speech_data = { command = DisplaySpeech { text_to_display = msg } ; is_finished = finished } in
  Channel.send_msg speech speech_data;
  while not !finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done

let set_state (s : RegionAgentState.t) : unit =
  raise (RegionAgentState.ChangeState s)
