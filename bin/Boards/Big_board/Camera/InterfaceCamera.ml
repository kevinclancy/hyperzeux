
open Common
open BoardInterface
open CameraAgentState

type speech_state =
  | SpeechVisible of (bool ref)
  (** [SpeechVisible(is_finished)] A speech box is visible.
      [is_finished] should be set to true when the user acknowledges they have finished reading it. *)
  | NoSpeechVisible
  (** Speech is not currently visible. *)

type camera_fields = {
  mutable speech_state : speech_state ;
}

let state_speech : camera_fields CameraAgentState.blueprint = {
  state_functions = {
    CameraAgentState.empty_state_functions with
      create_handlers = Some(fun fields ->
        let open Shared.Channels.Speech in
        [
          Channel.attach_handler speech (fun speech_command board ->
            match speech_command with
            | DisplaySpeech (lines, is_finished) ->
              board.draw_text "speech_box" lines;
              fields.speech_state <- SpeechVisible(is_finished);
              None
            | BeginSpeech (lines, is_finished) ->
              board.draw_text "speech_box" lines;
              fields.speech_state <- SpeechVisible(is_finished);
              None
            | EndSpeech ->
              fields.speech_state <- NoSpeechVisible;
              None
          )
        ]
      );

      key_space_pressed = Some(fun board fields ->
        match fields.speech_state with
        | SpeechVisible(is_finished) ->
          is_finished := true;
          None
        | NoSpeechVisible ->
          None
      );

      get_viewports = (fun (fields : camera_fields) ->
        match fields.speech_state with
        | SpeechVisible(_) ->
          [({ layer = "user_interface" ; pos = vec2 0. 0. ; scale = 3. })]
        | NoSpeechVisible ->
          []
      ) ;
  };

  props = {
    name = "speech";
  };
}

let camera : CameraAgent.camera_agent_class = {
  states = StringMap.of_list [
    ("speech", state_speech.props)
  ];

  initial_state = CameraAgentState.create state_speech { speech_state = NoSpeechVisible } ;

  name = "InterfaceCamera"
}