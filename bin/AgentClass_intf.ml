module type AgentClass = sig
  (** [create name pos] Creates an agent named [name] at position [pos] *)
  val create : string -> Common.position -> Agent.t
end
