module type Agent = sig
  val resume : unit -> Actions.action
end

module type Walker = sig
  val walk_north : unit -> unit Effect.t
  val walk_east : unit -> unit Effect.t
  val walk_south : unit -> unit Effect.t
  val walk_west : unit -> unit Effect.t
end