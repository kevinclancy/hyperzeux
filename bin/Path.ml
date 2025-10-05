open Common

type path = {
  mutable description : string ;
  (** A description of the path *)

  mutable nodes : pre_position list
  (** The sequence of grid cell positions that make up this path *)
}
