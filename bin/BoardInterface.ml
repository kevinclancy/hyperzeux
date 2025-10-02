open Common

type board_interface = {
  get_waypoint : string -> position;
  (** Retrieve the waypoint with the given name *)

  get_region : string -> Region.region ;
  (** Retrieve the region with the given name *)

  get_puppet : string -> Puppet.t ;
  (** Retrieve the puppet with the given name *)

  draw_text : string -> string -> unit
  (** [draw_text region_name text] draws [text] into the region named [region_name].
      Precondition: the region must have exactly one component. *)
}