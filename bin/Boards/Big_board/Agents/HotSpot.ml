open RegionAgent
open BoardInterface
open Common

let script (board : board_interface) (puppet : PuppetExternal.t) =
  let open Shared.Channels.Speech in
  say "hello world";
  say "goodbye world"

let hotspot_class : region_agent_class =
  let open Shared.RegionAgentCreators in
  Trigger.create "Hotspot" script