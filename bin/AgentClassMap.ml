open AgentClass_intf

open Common

(** Maps each agent class name to the appropriate agent class *)
let map : (module AgentClass) StringMap.t ref = ref StringMap.empty

let add (class_name : string) (agent_class : (module AgentClass)) =
  map := StringMap.add class_name agent_class !map

let get (class_name : string) : (module AgentClass) =
  StringMap.find class_name !map