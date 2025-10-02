open Common
open AmbientAgent
open AmbientAgentState

let speech_box_width = Config.screen_width_f /. 3.0
let speech_box_height = 300.0
let speech_box_x = (Config.screen_width_f /. 2.0) -. (speech_box_width /. 2.0)

let speech_box_y = Config.screen_height_f -. speech_box_height -. 10.

let rec hidden_state : unit AmbientAgentState.blueprint = {
  state_functions = {
    AmbientAgentState.empty_state_functions with
      create_handlers = Some(fun () ->
        [
          Channel.attach_handler (Channels.Speech.speech) (fun speech_data board ->
            match speech_data.command with
            | Channels.Speech.DisplaySpeech { text_to_display } ->
              Some(AmbientAgentState.create visible_state (text_to_display, speech_data.is_finished))
            | _ ->
              None
          )
        ]
      );
  };

  props = {
    name = "Hidden"
  }
}

and visible_state : (string * bool ref) AmbientAgentState.blueprint = {
  state_functions = {
    AmbientAgentState.empty_state_functions with
      key_space_pressed = Some(fun board ((msg, finished) : string * bool ref) ->
        finished := true;
        Some(AmbientAgentState.create hidden_state ())
      );
      draw = Some(fun ((msg, finished) : string * bool ref) ->
        let open Raylib in
        let rect = Rectangle.create speech_box_x speech_box_y speech_box_width speech_box_height in
        draw_rectangle_rec rect Color.purple;
        let font = FontMap.get "romulus.png" in
        Raylib.draw_text_ex font msg (Vector2.create (speech_box_x +. 5.) (speech_box_y +. 5.)) 24. 2. (Raylib.Color.black)
      )
  };

  props = {
    name = "Visible"
  }
}

let speech_box_class : ambient_agent_class = {
  states = StringMap.of_list [
    ("Hidden", hidden_state.props);
    ("Visible", visible_state.props)
  ];

  initial_state = AmbientAgentState.create hidden_state ();

  speed = 1.0 ;

  name = "SpeechBox"
}