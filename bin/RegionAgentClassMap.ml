open RegionAgent

include OrdMap.Make(
  struct
    type a = region_agent_class

    let get_name (agent_class : region_agent_class) =
      agent_class.name
  end
)