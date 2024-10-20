open Common

module Blueprint : sig
    type t
    (** A static specification of a board's starting state,
        created using the level designer. *)

    val create_empty : int -> int -> string -> t
    (** [create_empty width height empty_object_key] Creates an empty board blueprint
        of [width] by [height] cells, filling each cell with the static object whose index is
        [empty_obj_key]. *)


    val set_static_object : t -> position -> string -> Raylib.Color.t -> unit
    (** [set_static_object_key blueprint pos key color] Set position [pos] to static object key
        [key] and color [color] *)

    val add_agent : t -> string -> Raylib.Color.t -> string -> string -> position -> unit
    (** [set_agent blueprint agent_name color agent_class_name texture_name pos] *)

    val contains_agent_name : t -> string -> bool
    (** [contains_agent_name name] returns true iff this board blueprint
        contains a blueprint for an agent named [name] *)

    val draw_prep : t -> unit
    (** [draw_prep blueprint] Draws board components to a render texture in preparation for drawing *)

    val draw : t -> Raylib.Vector2.t -> float -> unit
    (** [draw blueprint pos scale] *)
end

type t

val create_empty : int -> int -> static_object -> t
(** [create_empty width height empty_object] Creates an empty board [width] cells across and [height] cells high.
    Each cells has [empty_object] as a static object. *)

val create_from_blueprint : Blueprint.t -> t
(** [create_from_blueprint blueprint] Creates a board from [blueprint] *)

val set_static_object : t -> position -> static_object -> unit
(** [set_static_object board pos static_object] Sets the static object to [static_object]
    at position [pos] on [board] *)

val add_agent : t -> Agent.t -> unit
(** [add_agent board agent] adds [agent] to [board] assuming [agent]'s position is
    vacant and no agent with a matching name already exists in [board]. *)

val prep_draw : t -> unit
(** [prep_draw board] Prepare board for drawing *)

val draw : t -> Raylib.Vector2.t -> float -> unit
(** [draw board pos scale] Draw the board positioned at [pos] scaled by [scale] *)

val update : t -> unit
(** [update board] Updates all agents on the board *)