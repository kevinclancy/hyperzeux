open Agent

let map = OrdMap.create (fun (m : agent_class) -> m.name)

let add = OrdMap.add map

let get = OrdMap.get map

let get_first_elem () = OrdMap.get_first_elem map

let get_next_elem = OrdMap.get_next_elem map

let get_prev_elem = OrdMap.get_prev_elem map

let search = OrdMap.search map
