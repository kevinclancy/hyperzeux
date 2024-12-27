open Common

type t
(** The interface that an agent's scripts have to other agents *)

val from : Puppet.t -> t
(** [from p] Wraps a puppet [p] in an external interface *)

val get_pos : t -> position
(** Get the current position of the agent *)

val get_name : t -> string
(** Get the name of the agent *)