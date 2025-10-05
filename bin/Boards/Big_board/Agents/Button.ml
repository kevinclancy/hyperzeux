open Common
open Actions
open Agent
open AgentState
open BoardInterface
open Shared.Channels

let script (board : board_interface) (self : Puppet.t) : unit =
  let open Shared.AgentScriptFunctions in
  say self ["hello world"];
  say self ["goodbye world"]

let button_class : agent_class =
  let open Shared.AgentCreators in
  BumpBot.create "button" "person_south_recon.png" Raylib.Color.red script