open Common
type t

(** A static specification of a board's starting state,
    created using the level designer. *)
module Blueprint : sig
    type t

    (** [create_empty width height empty_object_key] Creates an empty board blueprint
        of [width] by [height] cells, filling each cell with the static object whose index is
        [empty_obj_key]. *)
    val create_empty : int -> int -> string -> t

    (** [set_static_object_key blueprint pos key color] Set position [pos] to static object key
        [key] and color [color] *)
    val set_static_object : t -> position -> string -> Raylib.Color.t -> unit

    (** [set_agent blueprint agent_name agent_class_name texture_name pos] *)
    val add_agent : t -> string -> string -> string -> position -> unit

    (** [draw_prep blueprint] Draws board components to a render texture in preparation for drawing *)
    val draw_prep : t -> unit

    (** [draw blueprint pos scale] *)
    val draw : t -> Raylib.Vector2.t -> float -> unit

end

(** [create_empty width height empty_object] Creates an empty board [width] cells across and [height] cells high.
    Each cells has [empty_object] as a static object. *)
val create_empty : int -> int -> static_object -> t

(** [create_from_blueprint blueprint] Creates a board from [blueprint] *)
val create_from_blueprint : Blueprint.t -> t

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