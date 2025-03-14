
let curr_class = ref None

let get () : CameraAgent.camera_agent_class =
  Option.get !curr_class

let set (camera_class : CameraAgent.camera_agent_class) : unit =
  curr_class := Some(camera_class)