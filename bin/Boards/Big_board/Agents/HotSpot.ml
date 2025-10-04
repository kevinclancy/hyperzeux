open RegionAgent
open BoardInterface
open Common

let script (board : board_interface) (puppet : PuppetExternal.t) =
  let open Shared.RegionScriptFunctions in

  let command_patroller_chan = acquire "patroller" in

  begin_speech [
    "hello world. i am so cool.";
    "this is a world. world is world is world.";
    "blah balalala"
  ];
  perform_command command_patroller_chan (fun self board_intf command_fns ->
    for i = 1 to 10 do
      command_fns.walk_east self
    done;
  );
  say [
    "goodbye world";
    "goodbye world"
  ];
  end_speech ()

let hotspot_class : region_agent_class =
  let open Shared.RegionAgentCreators in
  Trigger.create "Hotspot" script