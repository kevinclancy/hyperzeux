open Actions

type speech_command =
  | BeginSpeech of string list * bool ref
  (** [BeginSpeech(initial_text_lines, is_finished)] Spawns a speech bubble displaying [initial_text_lines].
      [is_finished] becomes true when the user acknowledges they are finished reading the speech. *)
  | DisplaySpeech of string list * bool ref
  (** [DisplaySpeech(text_lines, is_finished)] displays [text_lines] in the speech bubble.
      [is_finished] becomes true when the user acknowledges they are finished reading the speech. *)
  | EndSpeech
  (** [EndSpeech] hides the speech bubble *)

let speech : speech_command Channel.t = Channel.create "speech"
(** A channel for modifying the current speech bubble *)