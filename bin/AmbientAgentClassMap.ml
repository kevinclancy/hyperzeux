open AmbientAgent

include OrdMap.Make(
  struct
    type a = ambient_agent_class

    let get_name (agent_class : ambient_agent_class) =
      agent_class.name
  end
)