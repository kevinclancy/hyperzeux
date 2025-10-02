let say (msg : string) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (DisplaySpeech (msg, finished));
  while not !finished do
    ignore (Effect.perform @@ Actions.Act Actions.Wait)
  done

let begin_speech (msg : string) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (BeginSpeech (msg, finished));
  while not !finished do
    ignore (Effect.perform @@ Actions.Act Actions.Wait)
  done

let end_speech () : unit =
  let open Channels.Speech in
  Channel.send_msg speech EndSpeech

let set_state (s : AgentState.t) : unit =
  raise (AgentState.ChangeState s)