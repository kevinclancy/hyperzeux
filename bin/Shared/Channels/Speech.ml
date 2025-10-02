open Actions

type speech_command =
  | BeginSpeech
  (** Display the speech bubble *)
  | DisplaySpeech of { text_to_display : string }
  (** Display some text in the speech bubble *)
  | EndSpeech
  (** Hide the speech bubble *)

type speech_data = {
  command : speech_command ;
  (** The command to perform *)
  is_finished : bool ref
  (** Starts false and becomes true when the command has finished. *)
}

let speech : speech_data Channel.t = Channel.create "speech"
(** A channel for modifying the current speech bubble *)