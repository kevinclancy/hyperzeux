module Speech = struct
  open Actions

  type speech_command =
    | BeginSpeech of string list * Puppet.t option * bool ref
    (** [BeginSpeech(initial_text_lines, speaker, is_finished)] Spawns a speech bubble displaying [initial_text_lines].
        [speaker] if present is used to display the speaker puppet's name and other characteristics
        [is_finished] becomes true when the user acknowledges they are finished reading the speech. *)
    | DisplaySpeech of string list * Puppet.t option * bool ref
    (** [DisplaySpeech(text_lines, speaker, is_finished)] displays [text_lines] in the speech bubble.
        [speaker] if present is used to display the speaker puppet's name and other characteristics
        [is_finished] becomes true when the user acknowledges they are finished reading the speech. *)
    | EndSpeech
    (** [EndSpeech] hides the speech bubble *)

  let speech : speech_command Channel.t = Channel.create "speech"
  (** A channel for modifying the current speech bubble *)
end

module Music = struct
  type music_command =
    | PlaySong of string
    (** [PlaySong filename] starts playing the mp3 file at [filename] *)
    | PerformFade of float * float * float * bool ref
    (** [PerformFade (start_volume, end_volume, duration, is_finished)]
        performs a volume fade from [start_volume] to [end_volume] over [duration] seconds.
        Sets [is_finished] to true when finished *)
    | StopSong
    (** [StopSong] stops the current song being played *)

  let music : music_command Channel.t = Channel.create "music"
  (** A channel for controlling music playback *)
end

(** Acquire channel for when the ruffian sees the player walk into the green cup *)
let world_camera_acquire : CameraStateCreators.AcquiredCameraState.acquire_msg Channel.t =
  Channel.create "world_camera_acquire"
