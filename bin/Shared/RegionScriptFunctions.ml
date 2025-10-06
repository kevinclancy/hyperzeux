let say (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (DisplaySpeech (lines, None, finished));
  while not !finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done

let begin_speech (lines : string list) : unit =
  let open Channels.Speech in
  let finished = ref false in
  Channel.send_msg speech (BeginSpeech (lines, None, finished));
  while not !finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done

let end_speech () : unit =
  let open Channels.Speech in
  Channel.send_msg speech EndSpeech

let acquire (acquire_channel : AgentStateCreators.AcquiredState.acquire_msg Channel.t) : AgentStateCreators.AcquiredState.command_channel =
  (** [acquire acquire_channel] acquires an agent by sending a message on its acquire channel.
      Returns the command channel for sending commands to the acquired agent. *)

  let ref_opt_command_channel = ref None in
  Channel.send_msg acquire_channel ref_opt_command_channel;
  while Option.is_none !ref_opt_command_channel do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  Option.get !ref_opt_command_channel

let acquire_camera (acquire_channel : CameraStateCreators.AcquiredCameraState.acquire_msg Channel.t) : CameraStateCreators.AcquiredCameraState.command_channel =
  (** [acquire_camera acquire_channel] acquires a camera agent by sending a message on its acquire channel.
      Returns the command channel for sending commands to the acquired camera. *)

  let ref_opt_command_channel = ref None in
  Channel.send_msg acquire_channel ref_opt_command_channel;
  while Option.is_none !ref_opt_command_channel do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  Option.get !ref_opt_command_channel

let send_command
  (command_channel : AgentStateCreators.AcquiredState.command_channel)
  (script_fn : AgentStateCreators.AcquiredState.script_fn)
  : bool ref =

  (** [send_command command_channel script_fn] sends a command along a command channel,
      and returns a reference that becomes true when the command has finished executing *)

  let open AgentStateCreators.AcquiredState in
  let is_finished = ref false in
  Channel.send_msg command_channel (Command (script_fn, is_finished));
  is_finished

let perform_command
  (command_channel : AgentStateCreators.AcquiredState.command_channel)
  (script_fn : AgentStateCreators.AcquiredState.script_fn)
  : unit =

  (** [perform_command command_channel script_fn] sends a command along a command channel, and returns once the
      command has finished executing. *)

  let is_finished = send_command command_channel script_fn in
  while not !is_finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  ()

let cam_perform_command
  (command_channel : CameraStateCreators.AcquiredCameraState.command_channel)
  (script_fn : BoardInterface.board_interface -> CameraStateCreators.AcquiredCameraState.camera_fields -> unit)
  : unit =

  (** [cam_perform_command command_channel script_fn] sends a command along a camera command channel, and returns once the
      command has finished executing. *)

  let open CameraStateCreators.AcquiredCameraState in
  let is_finished = ref false in
  Channel.send_msg command_channel (Command (script_fn, is_finished));
  while not !is_finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  ()

let unacquire (command_channel : AgentStateCreators.AcquiredState.command_channel) : unit =
  (** [unacquire command_channel] releases the acquired agent, returning it to its return state *)

  let open AgentStateCreators.AcquiredState in
  let is_finished = ref false in
  Channel.send_msg command_channel (Release is_finished);
  while not !is_finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  ()

let unacquire_camera (command_channel : CameraStateCreators.AcquiredCameraState.command_channel) : unit =
  (** [unacquire_camera command_channel] releases the acquired camera, returning it to its return state *)

  let open CameraStateCreators.AcquiredCameraState in
  let is_finished = ref false in
  Channel.send_msg command_channel (Release is_finished);
  while not !is_finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  ()

let with_acquired
  (agent_acquire_channels : AgentStateCreators.AcquiredState.acquire_msg Channel.t list)
  (camera_acquire_channels : CameraStateCreators.AcquiredCameraState.acquire_msg Channel.t list)
  (f : AgentStateCreators.AcquiredState.command_channel list -> CameraStateCreators.AcquiredCameraState.command_channel list -> unit)
  : unit =
  (** [with_acquired agent_acquire_channels camera_acquire_channels f] acquires multiple agents and cameras by their acquire channels,
      calls [f] with their command channels, and automatically releases all agents and cameras when [f] completes
      (even if an exception occurs). *)

  let agent_command_channels = List.map acquire agent_acquire_channels in
  let camera_command_channels = List.map acquire_camera camera_acquire_channels in
  Fun.protect
    ~finally:(fun () ->
      List.iter unacquire agent_command_channels;
      List.iter unacquire_camera camera_command_channels)
    (fun () -> f agent_command_channels camera_command_channels)

let set_state (s : RegionAgentState.t) : unit =
  raise (RegionAgentState.ChangeState s)

let play_music_fade_in (filename : string) (fade_duration : float) : unit =
  (** [play_music_fade_in filename fade_duration] loads and plays a music file with a fade-in effect. *)

  let open Channels.Music in
  let is_finished = ref false in
  Channel.send_msg music (PlaySong filename);
  Channel.send_msg music (PerformFade (0.0, 1.0, fade_duration, is_finished))

let start_music (filename : string) : unit =
  (** [start_music filename fade_duration] starts playing the mp3 at [filename]. *)

  let open Channels.Music in
  Channel.send_msg music (PlaySong filename)

let stop_music_fade_out (fade_duration : float) : unit =
  (** [stop_music_fade_out fade_duration] fades out and stops the music. *)

  let open Channels.Music in
  let is_finished = ref false in
  Channel.send_msg music (PerformFade (1.0, 0.0, fade_duration, is_finished));
  while not !is_finished do
    ignore (Effect.perform @@ RegionAgentState.RegionAction ())
  done;
  Channel.send_msg music StopSong
