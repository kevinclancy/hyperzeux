open Common
open Region

type waypoint = {
    name : string ;
    (** the name of the waypoint *)

    position : position ;
    (** the position of the waypoint *)

    description : string ;
    (** the description of the waypoint *)
}


module Blueprint : sig
    type t
    (** A static specification of a board's starting state,
        created using the level designer. *)

    type edit_state
    (** A state of the blueprint editor *)

    val create_empty : unit -> t
    (** [create_empty ()] Creates an empty board blueprint *)

    val create_initial_state : int -> int -> string -> edit_state
    (** [create_initial state width height blank_char_name] Creates an initial editor state whose initial layer
        has dimensions [width] and [height] and whose cells are filled with the Texture identified by [blank_char_name] *)

    val add_layer : edit_state -> string -> int -> int -> unit
    (** [add_layer edit_state name width height] adds a new layer named [name] that is [width] cells
        across and [height] cells high, filled with empty cells  *)

    val select_layer : edit_state -> string -> unit
    (** [select_layer edit_state layer_name] Selects [layer_name] as the current layer*)

    val get_width : edit_state -> int
    (** Get the number of columns in the currently edited layer *)

    val get_height : edit_state -> int
    (** Get the number of rows in the currently edited layer *)

    val get_blueprint : edit_state -> t
    (** Get the currently edited blueprint *)

    val get_layer_names : edit_state -> string list
    (** Gets a list of all layer names *)

    val get_current_layer_name : edit_state -> string
    (** Return the name of the currently edited layer *)

    val set_static_object : edit_state -> pre_position -> string -> Raylib.Color.t -> unit
    (** [set_static_object_key blueprint pos key color] Set position [pos] to static object key
        [key] and color [color] *)

    val get_static_object_name : edit_state -> pre_position -> string
    (** [get_static_object_name blueprint pos] Returns name of static object at position [pos] *)

    val get_static_object_color : edit_state -> pre_position -> Raylib.Color.t
    (** [get_static_object blueprint pos] Returns static object at position [pos] *)

    val add_agent : edit_state -> string -> Raylib.Color.t -> string -> string -> pre_position -> unit
    (** [add_agent blueprint agent_name color agent_class_name texture_name pos] *)

    val add_ambient_agent : edit_state -> string -> string -> unit
    (** [add_ambient_agent blueprint ambient_name ambient_class_name] *)

    val remove_ambient_agent : t -> string -> unit
    (** [remove_ambient_agent] *)

    val add_waypoint : edit_state -> string -> pre_position -> string -> unit
    (** [add_waypoint blueprint waypoint_name waypoint_position description]
        Adds a waypoint named [waypoint_name] at position [waypoint_position] with description [description] *)

    val contains_pos : edit_state -> pre_position -> bool
    (** [contains_pos blueprint pos] Returns true iff [pos] is a valid cell position of [blueprint] *)

    val contains_waypoint_name : edit_state -> string -> bool
    (** [contains_waypoint_name blueprint name] *)

    val get_waypoints : edit_state -> waypoint StringMap.t
    (** Get the blueprint's map from waypoint name to waypoint *)

    val set_waypoints : edit_state -> waypoint StringMap.t -> unit
    (** Set the blueprint's map from waypoint name to waypoint *)

    val contains_agent_name : edit_state -> string -> bool
    (** [contains_agent_name blueprint name] returns true iff this board blueprint
        contains a blueprint for an agent named [name] *)

    val contains_ambient_name : t -> string -> bool
    (** [contains_ambient_name blueprint name] returns true iff the board blueprint
        contains a blueprint for an ambient named [name] *)

    val region_names : edit_state -> string list
    (** [region_names s] Return a list of all region names in the currently edited blueprint *)

    val ambient_names : edit_state -> string list
    (** [ambient_names s] Returns a list of all ambient agent names in the currently edited blueprint *)

    val get_region : edit_state -> string -> region
    (** [region s region_name] Returns the region in the currently edited blueprint whose name is [region_name] *)

    val add_region : edit_state -> string -> region -> unit
    (** [add_region s region_name region] Adds [region] with name [region_name] to region map *)

    val del_region : edit_state -> string -> unit
    (** [del_region state region_name] Deletes region with specified name *)

    val draw_prep : edit_state -> unit
    (** [draw_prep blueprint] Draws board components to a render texture in preparation for drawing *)

    val draw : edit_state -> Raylib.Vector2.t -> float -> unit
    (** [draw blueprint pos scale] *)

    val bp_to_edit_state : t -> edit_state
    (** [bp_to_edit_state bp] converts bp to an edit state *)

    val serialize : t -> string -> unit
    (** [serialize blueprint filename] Saves the board blueprint to the file whose absolute path is [filename] *)

    val deserialize : string -> t
    (** [deserialize board_file] Loads a board blueprint from the absolute file path [board_file] *)
end

type t

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

val draw : t -> unit
(** [draw board] Draw the board (the scale and camera position for each layer are determined by the camera agent) *)

val update : t -> unit
(** [update board] Updates all agents on the board *)