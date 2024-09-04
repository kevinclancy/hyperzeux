open AgentClass_intf

val add : string -> (module AgentClass) -> unit

val get : string -> (module AgentClass)