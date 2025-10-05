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
