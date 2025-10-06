open Common
open BoardInterface
open CameraAgentState
open Shared.CameraStateCreators.AcquiredCameraState

(* Create the acquired camera state kit using the channel from Channels module *)
let acquired_kit = create Channels.world_camera_acquire

let state_following_player : camera_fields CameraAgentState.blueprint = {
  state_functions = {
    CameraAgentState.empty_state_functions with
      script = Some(fun (board : board_interface) (t_delta : float ref) (fields : camera_fields) ->
        let open Raylib in
        while true do
          let player_cell_pos = Puppet.get_pos @@ board.get_puppet "player" in
          let viewport_width = Config.screen_width_f /. fields.scale in
          let viewport_height = Config.screen_height_f /. fields.scale in
          let camera_player_offset = vec2 (viewport_width /. 2.) (viewport_height /. 2.) in
          let ideal_pos = (disc_to_cont player_cell_pos) ^- camera_player_offset in
          let max_camera_x = Config.board_pixels_width_f -. viewport_width in
          let max_camera_y = Config.board_pixels_height_f -. viewport_height in
          fields.pos <-
            vec2
              (min (max (Vector2.x ideal_pos) 0.) max_camera_x)
              (min (max (Vector2.y ideal_pos) 0.) max_camera_y);
          ignore (Effect.perform @@ CameraAction ())
        done
      );

      get_viewports = (fun (fields : camera_fields) ->
        [({ layer = "main" ; pos = fields.pos ; scale = fields.scale })]) ;

      create_handlers = Some(fun fields ->
        [acquired_kit.acquire_handler fields]
      );
  };

  props = {
    name = "FolowingPlayer";
  };
}

let () =
  let initial_fields = { pos = vec2 0. 0. ; scale = 4. } in
  acquired_kit.set_return_state (CameraAgentState.create state_following_player initial_fields)

let camera : CameraAgent.camera_agent_class =
  let initial_fields = { pos = vec2 0. 0. ; scale = 4. } in
  acquired_kit.add_acquire_states {
    states = StringMap.of_list [
      ("FollowingPlayer", state_following_player.props)
    ];

    initial_state = CameraAgentState.create state_following_player initial_fields;

    name = "Camera"
  }