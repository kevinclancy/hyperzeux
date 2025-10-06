open Common

(** Acquire channel for when the ruffian sees the player walk into the green cup *)
let ruffian_sees_greencup : Shared.AgentStateCreators.AcquiredState.acquire_msg Channel.t =
  Channel.create "ruffian_sees_greencup"

(** Acquire channel for the world camera *)
let world_camera_acquire : Shared.CameraStateCreators.AcquiredCameraState.acquire_msg Channel.t =
  Channel.create "world_camera_acquire"
