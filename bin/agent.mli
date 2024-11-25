open Common

type t

type event_handlers = {
    initial : (t -> unit) option ;

    receive_bump : (t -> t -> unit) option ;
    (** [receive_bump self other] is called when [other] bumps into this agent *)

    assert_invariants : (t -> unit) option ;

    key_left : (t -> unit) option ;

    key_right : (t -> unit) option ;

    key_up : (t -> unit) option ;

    key_down : (t -> unit) option
}

val empty_scripts : event_handlers

val get_pos : t -> position

val set_pos : t -> position -> unit

val get_texture : t -> Raylib.Texture.t

val set_texture : t -> Raylib.Texture.t -> unit

val get_name : t -> string

val get_color : t -> Raylib.Color.t
(** Get the current color that the agent is displayed in *)

(* do I need to include a texture as an argument below, or could new agents just use some default tex? *)

val create : string -> event_handlers -> position -> Raylib.Color.t -> ?speed:float -> Raylib.Texture.t -> t
(** [create board_intf name script pos texture] Creates a new agent executing [scripts] starting at [pos] *)

val resume : t -> Actions.action_result -> Actions.action
(** [resume agent prev_action_result] Resumes [agent]'s script, providing the result of its previous action
    [prev_action_result] to the script's current context. *)

val update_input : t -> unit
(** Update agent state in response to user input *)

val receive_bump : t -> t -> unit
(** [receive_bump self other] called when [other] bumps into [self] *)