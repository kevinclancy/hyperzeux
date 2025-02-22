open Common
open Actions
open Agent
open AgentState
open BoardInterface
open Channels

let say (msg : string) : unit =
  let finished = ref false in
  Channel.send_msg Channels.Common.speech (msg, finished);
  while not !finished do
    ignore (Effect.perform @@ Act Wait)
  done

let script (board : board_interface) (me : Puppet.t) : unit =
  say "hello world";
  say "goodbye world"

let button_class : agent_class = AgentMakers.bump_bot "button" "person_south_recon.png" Raylib.Color.red script