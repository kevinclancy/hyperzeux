open CameraAgent

include OrdMap.Make(
  struct
    type a = camera_agent_class

    let get_name (camera_class : camera_agent_class) =
      camera_class.name
  end
)
