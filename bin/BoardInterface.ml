open Common

type board_interface = {
  get_waypoint : string -> position;
  (** Retrieve the waypoint with the given name *)

  get_region : string -> Region.region
  (** Retrieve the region with the given name *)
}