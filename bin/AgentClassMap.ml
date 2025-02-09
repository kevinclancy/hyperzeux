open Agent

include OrdMap.Make(
  struct
    type a = agent_class

    let get_name (agent_class : agent_class) =
      agent_class.name
  end
)