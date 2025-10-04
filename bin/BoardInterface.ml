open Common

type board_interface = {
  get_waypoint : string -> position;
  (** Retrieve the waypoint with the given name *)

  get_region : string -> Region.region ;
  (** Retrieve the region with the given name *)

  get_puppet : string -> Puppet.t ;
  (** Retrieve the puppet with the given name *)

  draw_text : string -> string list -> unit
  (** [draw_text region_name text_lines] draws [text_lines] into the region named [region_name],
      where each element of [text_lines] is drawn on a subsequent line.
      Precondition: the region must have exactly one component. *)
}