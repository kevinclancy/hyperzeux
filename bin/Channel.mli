
type 'msg t
(** A channel whose message type is ['msg] *)

type ('res, 'handle_args) t_in_handler
(** An input destination of unknown message type, with an attached handler.
    The handler takes arguments of type ['handle_args] and produces results of type ['res].
    At most one message is handled on demand using [Channel.try_handle_one]. *)

val create : string -> 'msg t
(** [create_channel name] Creates a channel named [name] of message type ['msg] *)

val send_msg : 'msg t -> 'msg -> unit
(** [send_msg channel msg] Sends [msg] to all input ports for [channel] *)

val attach_handler : 'msg t  -> ('msg -> 'handle_args -> 'res) -> ('res, 'handle_args) t_in_handler
(** Create a new input destination for the channel and attach a handler *)

val handle_one : ('res, 'handle_args) t_in_handler -> 'handle_args -> 'res
(** Handle the next message in this input port's queue, returning the result of the handler.

    Precondition: Channel must be non-empty, which can be tested by calling [is_empty]  *)

val is_empty : ('res, 'handle_args) t_in_handler -> bool
(** [is_empty handler] Return true if there are no messages waiting in [handler] to be handled *)

val get_name : 'msg t -> string
(** [get_name channel] Returns the name of the channel *)
