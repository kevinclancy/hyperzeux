let speech : (string * bool ref) Channel.t = Channel.create "speech"
(** This channel displays whatever message is sent along it *)

let say (msg : string) : unit =
  let finished = ref false in
  Channel.send_msg speech (msg, finished);
  while not !finished do
    ()
  done