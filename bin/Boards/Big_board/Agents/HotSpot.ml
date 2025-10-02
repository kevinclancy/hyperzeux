open RegionAgent
open BoardInterface
open Common

let script (board : board_interface) (puppet : PuppetExternal.t) =
  let open Shared.RegionScriptFunctions in
  begin_speech "
hello world. i am so cool.
this is a world. world is world is world.
blah balalala
  ";
  say "
goodbye world
goodbye world
  ";
  end_speech ()

let hotspot_class : region_agent_class =
  let open Shared.RegionAgentCreators in
  Trigger.create "Hotspot" script