open RegionAgent
open BoardInterface
open Common

let script (board : board_interface) (puppet : PuppetExternal.t) =
  let open Global.RegionScriptFunctions in

  let acq_agents = [Channels.ruffian_sees_greencup] in
  let acq_cameras = [Global.Channels.world_camera_acquire] in

  with_acquired acq_agents acq_cameras (fun [ruffian] [camera] ->
    start_music "music/Mono.mp3";
    begin_speech [
      "the moment you walk near the ruffian";
      "he cowers in fear"
    ];
    perform_command ruffian (fun self board_intf command_fns ->
      let open Global.AgentScriptFunctions in
      say self [
        "Heellllp! A stranger! Leafy!" ;
        "Help me leafy!"
      ];
      end_speech ();
      command_fns.walk_to self board_intf "special_plant"
    );
    begin_speech [
      "The ruffian relaxes as he approaches";
      "the tuft of grass."
    ];
    end_speech ();
    cam_perform_command camera (fun board_intf cam_fields ->
      let open Global.CameraScriptFunctions in
      let player_pos = Some (to_prepos (PuppetExternal.get_pos puppet)) in
      pan_along_path "path1" 70.0 board_intf cam_fields ?before:player_pos ?after:player_pos
    );
    stop_music_fade_out 2.0
  )

let hotspot_class : region_agent_class =
  let open Global.RegionAgentCreators in
  Trigger.create "Hotspot" script