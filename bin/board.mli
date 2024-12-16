open Common

type region_component = {
  (** A rectangular subsection of a region *)

    top  :  int ;
    (** The row index of the top cell of the rectangle *)

    left :  int ;
    (** The column index of the leftmost cell of the rectangle *)

    bottom : int ;
    (** The row index of the bottom cell of the rectangle *)

    right : int ;
    (** The column index of the rightmost cell of the rectangle *)
  }

type region = {
  (** A set of board cells *)

  description : string ref ;
  (** A description of the region *)

  components : (region_component StringMap.t) ref
  (** The rectangles the constitute the region *)
}


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

    val add_waypoint : t -> string -> position -> unit
    (** [add_waypoint blueprint waypoint_name waypoint_position]
        Adds a waypoint named [waypoint_name] at position [waypoint_position] *)

    val contains_waypoint_name : t -> string -> bool
    (** [contains_waypoint_name blueprint name] *)

    val contains_agent_name : t -> string -> bool
    (** [contains_agent_name name] returns true iff this board blueprint
        contains a blueprint for an agent named [name] *)

    val region_names : t -> string list
    (** [region_names bp] Return a list of all region names in the blueprint [bp] *)

    val region : t -> string -> region
    (** [region bp region_name] Returns the region in blueprint [bp] whose name is [region_name] *)

    val add_region : t -> string -> region -> unit
    (** [add_region bp region_name region] Adds [region] with name [region_name] to region map *)

    val draw_prep : t -> unit
    (** [draw_prep blueprint] Draws board components to a render texture in preparation for drawing *)

    val draw : t -> Raylib.Vector2.t -> float -> unit
    (** [draw blueprint pos scale] *)

    val serialize : t -> string -> unit
    (** [serialize blueprint filename] Saves the board blueprint to the file whose absolute path is [filename] *)

    val deserialize : string -> t
    (** [deserialize board_file] Loads a board blueprint from the absolute file path [board_file] *)
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