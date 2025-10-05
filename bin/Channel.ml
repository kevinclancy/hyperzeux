type 'msg t = {
  name : string ;
  (** The name of this channel *)

  queues : ('msg Queue.t) Weak.t
  (** A weak array of all queues of ports subscribing to this channel *)
}

type ('res, 'handle_args) t_in_handler = {
  handler_func : 'handle_args -> 'res ;
  is_empty : unit -> bool
}

let create (name : string) : 'msg t =
  {
    name;
    queues = Weak.create 14
  }

let send_msg (channel : 'msg t) (msg : 'msg) : unit =
  for i = 0 to (Weak.length channel.queues) - 1 do
    Option.iter (fun q -> Queue.push msg q) (Weak.get channel.queues i);
  done

let allocate_new_queue (channel : 'msg t) : 'msg Queue.t =
  let opt_queue = ref None in
  while Option.is_none !opt_queue do
    for i = 0 to (Weak.length channel.queues) - 1 do
      let new_queue = Some(Queue.create ()) in
      opt_queue := new_queue;
      Weak.set channel.queues i new_queue;
    done;
    if Option.is_none !opt_queue then
      Gc.major ();
  done;
  assert (Option.is_some !opt_queue);
  Option.get !opt_queue

let attach_handler (channel : 'msg t)
                   (handler_func : 'msg -> 'handle_args -> 'res)
                   : ('res, 'handle_args) t_in_handler =
  let queue = allocate_new_queue channel in
  let handler (args : 'handle_args) : 'res =
    handler_func (Queue.pop queue) args
  in
  { handler_func = handler ; is_empty = fun () -> Queue.is_empty queue }

let handle_one (handler : ('res, 'handle_args) t_in_handler) (args : 'handle_args) : 'res =
  assert (not @@ handler.is_empty ());
  handler.handler_func args

let is_empty (handler : ('res, 'handle_args) t_in_handler) : bool =
  handler.is_empty ()

let get_name (channel : 'msg t) : string =
  channel.name