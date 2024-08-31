open Common

type t

(** [create width height empty_object] Creates an empty board [width] cells across and [height] cells high.
    Each cells has [empty_object] as a static object. *)
val create_empty : int -> int -> static_object -> t

(** [set_static_object board pos static_object] Sets the static object to [static_object]
    at position [pos] on [board] *)
val set_static_object : t -> position -> static_object -> unit

(** [add_agent board agent] adds [agent] to [board] assuming [agent]'s position is
    vacant and no agent with a matching name already exists in [board]. *)
val add_agent : t -> Agent.t -> unit

(** [draw board pos scale] Draw the board positioned at [pos] scaled by [scale] *)
val draw : t -> Raylib.Vector2.t -> float -> unit

(** [update board] Updates all agents on the board *)
val update : t -> unit