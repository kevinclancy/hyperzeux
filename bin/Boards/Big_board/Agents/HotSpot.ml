open RegionAgent
open BoardInterface
open Common

let script (board : board_interface) (puppet : PuppetExternal.t) =
  let open Shared.RegionScriptFunctions in

  let command_patroller_chan = acquire "ruffian" in

  begin_speech [
    "the moment you walk into the green";
    "cup, a strange ruffian nearby explodes";
    "into a fit of anger"
  ];
  perform_command command_patroller_chan (fun self board_intf command_fns ->
    let open Shared.AgentScriptFunctions in
    say self [
      "You fool! Walking into the green cup" ;
      "could destroy my special plant!"
    ];
    end_speech ();
    command_fns.walk_to self board_intf "special_plant"
  );
  begin_speech [
    "The man saw that his plant was okay";
    "and became visibly calm."
  ];
  end_speech ()

let hotspot_class : region_agent_class =
  let open Shared.RegionAgentCreators in
  Trigger.create "Hotspot" script