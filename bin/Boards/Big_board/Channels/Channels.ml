open Common

(** Acquire channel for when the ruffian sees the player walk into the green cup *)
let ruffian_sees_greencup : Shared.AgentStateCreators.AcquiredState.acquire_msg Channel.t =
  Channel.create "ruffian_sees_greencup"
