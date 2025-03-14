
open Common
open BoardInterface
open CameraAgentState

let state_following_player : (vec2 ref) CameraAgentState.blueprint = {
  state_functions = {
    CameraAgentState.empty_state_functions with
      script = Some(fun (board : board_interface) (t_delta : float ref) (curr_pos : vec2 ref) ->
        while true do
          ignore (Effect.perform @@ CameraAction ())
        done
      );

      get_position = (fun (pos : vec2 ref) -> !pos)
  } ;

  props = {
    name = "FolowingPlayer"
  }
}

let camera : CameraAgent.camera_agent_class = {
  states = StringMap.of_list [
    ("FollowingPlayer", state_following_player.props)
  ];

  initial_state = CameraAgentState.create state_following_player (ref @@ vec2 (-300.0) (-10.0)) ;

  speed = 1.0;

  name = "Camera"
}