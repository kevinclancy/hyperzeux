open Common
open CameraAgentState
open BoardInterface

type command_msg =
  | Command of (board_interface -> camera_fields -> unit) * bool ref
  (** A command: a script function paired with a completion flag.
      The script function takes a board interface and camera fields.
      The bool ref is set to true when the command completes. *)
  | Release of bool ref
  (** A release message: tells the camera to return to its return state.
      The bool ref is set to true when the camera has transitioned. *)

and camera_fields = {
  mutable pos : vec2 ;
  mutable scale : float ;
}

type command_channel = command_msg Channel.t
(** A channel that transmits command messages *)

type acquire_msg = (command_channel option) ref
(** A message sent on an acquire channel.
    The sender passes a reference initialized to None.
    When the acquired camera enters the Acquired state, it creates a command channel
    and sets the reference to Some command_channel. *)

type acquired_camera_state_kit = {
  acquire_handler : camera_fields -> (CameraAgentState.t option, board_interface) Channel.t_in_handler ;
  (** A handler for the acquire channel that transitions the camera to the acquire state.
      Takes the camera fields from the current state. *)

  add_acquire_states : CameraAgent.camera_agent_class -> CameraAgent.camera_agent_class ;
  (** Add "Acquired" and "Running Script" states to the camera class *)

  set_return_state : CameraAgentState.t -> unit ;
  (** Set the return state for this acquired camera. Must be called before the camera is acquired. *)
}

let create (acquire_channel : acquire_msg Channel.t) : acquired_camera_state_kit =
  (** [create acquire_channel] creates an acquired camera state kit.
      [acquire_channel] is the channel that will be used to acquire this camera.
      The return state must be set later using set_return_state. *)

  let return_state_ref : CameraAgentState.t option ref = ref None in

  let rec acquired_state_blueprint (command_channel : command_channel) : camera_fields CameraAgentState.blueprint =
    {
      state_functions = {
        CameraAgentState.empty_state_functions with
        create_handlers = Some(fun fields ->
          [
            Channel.attach_handler command_channel (fun command_msg board_intf ->
              match command_msg with
              | Command (script, is_finished) ->
                Some (CameraAgentState.create (runscript_state_blueprint command_channel (script, is_finished)) fields)
              | Release is_finished ->
                is_finished := true;
                match !return_state_ref with
                | Some state -> Some state
                | None -> failwith "Camera was released before set_return_state was called"
            )
          ]
        );
        get_viewports = (fun (fields : camera_fields) ->
          [({ layer = "main" ; pos = fields.pos ; scale = fields.scale })]) ;
      } ;

      props = {
        name = "Acquired" ;
      }
    }
  and runscript_state_blueprint
    (command_channel : command_msg Channel.t)
    ((script, is_finished) : (board_interface -> camera_fields -> unit) * bool ref) : camera_fields CameraAgentState.blueprint = {

    state_functions = {
      CameraAgentState.empty_state_functions with

      script = Some (fun (board : board_interface) (t_delta : float ref) (fields : camera_fields) ->
        script board fields;
        is_finished := true;
        raise (CameraAgentState.ChangeState (CameraAgentState.create (acquired_state_blueprint command_channel) fields))
      ) ;

      create_handlers = Some (fun fields ->
        [
          (Channel.attach_handler
            command_channel
            (fun _ _ -> failwith "Received command while already running a script"))
        ]
      ) ;

      get_viewports = (fun (fields : camera_fields) ->
        [({ layer = "main" ; pos = fields.pos ; scale = fields.scale })]) ;
    } ;

    props = {
      name = "Running Script" ;
    }
  } in

  let channel_name = Channel.get_name acquire_channel in
  let acquire_handler (fields : camera_fields) =
    Channel.attach_handler acquire_channel (fun acquire_msg _ ->
      let command_channel = Channel.create ("command_" ^ channel_name) in
      acquire_msg := Some command_channel;
      Some (CameraAgentState.create (acquired_state_blueprint command_channel) fields)
    )
  in

  let add_acquire_states (camera_class : CameraAgent.camera_agent_class) : CameraAgent.camera_agent_class =
    let running_script_props = {
      name = "Running Script"
    } in
    let acquired_props = {
      name = "Acquired"
    } in
    {
      camera_class with
        states =
          camera_class.states
          |> StringMap.add "Running Script" running_script_props
          |> StringMap.add "Acquired" acquired_props
    }
  in

  let set_return_state (state : CameraAgentState.t) : unit =
    return_state_ref := Some state
  in

  {
    acquire_handler;
    add_acquire_states;
    set_return_state;
  }
