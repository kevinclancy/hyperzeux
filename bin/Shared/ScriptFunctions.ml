let set_state (s : AgentState.t) : unit =
  raise (AgentState.ChangeState s)