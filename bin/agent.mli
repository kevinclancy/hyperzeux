open Common

type t

type scripts = {
    initial : (t -> unit) option ;
    key_left : (t -> unit) option ;
    key_right : (t -> unit) option ;
    key_up : (t -> unit) option ;
    key_down : (t -> unit) option
}

val empty_scripts : scripts

val get_pos : t -> position

val set_pos : t -> position -> unit

val get_texture : t -> Raylib.Texture.t

val set_texture : t -> Raylib.Texture.t -> unit

val get_name : t -> string

val get_color : t -> Raylib.Color.t
(** Get the current color that the agent is displayed in *)

(* do I need to include a texture as an argument below, or could new agents just use some default tex? *)

val create : string -> scripts -> position -> Raylib.Color.t -> ?speed:float -> Raylib.Texture.t -> t
(** [create name script pos texture] Creates a new agent executing [scripts] starting at [pos] *)

val resume : t -> Actions.action

val update_input : t -> unit
(** Update agent state in response to user input *)