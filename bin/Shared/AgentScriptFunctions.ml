let say (msg : string) : unit =
  let open Channels.Speech in
  let finished = ref false in
  let speech_data = { command = DisplaySpeech { text_to_display = msg } ; is_finished = finished } in
  Channel.send_msg speech speech_data;
  while not !finished do
    ignore (Effect.perform @@ Actions.Act Actions.Wait)
  done

let set_state (s : AgentState.t) : unit =
  raise (AgentState.ChangeState s)