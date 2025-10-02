open Actions

type speech_command =
  | BeginSpeech of string * bool ref
  (** [BeginSpeech(initial_text, is_finished)] Spawns a speech bubble displaying [initial_text].
      [is_finished] becomes true when the user acknowledges they are finished reading the speech. *)
  | DisplaySpeech of string * bool ref
  (** [DisplaySpeech(text_to_display, is_finished)] displays [text_to_display] in the speech bubble.
      [is_finished] becomes true when the user acknowledges they are finished reading the speech. *)
  | EndSpeech
  (** [EndSpeech] hides the speech bubble *)

let speech : speech_command Channel.t = Channel.create "speech"
(** A channel for modifying the current speech bubble *)