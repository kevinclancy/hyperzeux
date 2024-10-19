open Common

(** [create name pos] Creates a patroller named [name] at position [pos] *)
val create : string -> position -> Agent.t

(** The name of this agent class: "Patroller" *)
val name : string