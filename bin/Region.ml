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