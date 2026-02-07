open AmbientAgent
open AmbientAgentState
open BoardInterface
open Common

type fade_state = {
  start_volume : float ;
  end_volume : float ;
  duration : float ;
  start_time : float ;
  is_finished : bool ref
}

type playing_music_fields = {
  mutable current_music : Raylib.Music.t ;
  (** The current music being played *)

  mutable current_volume : float ;
  (** The current volume level *)

  mutable fade_state : fade_state option ;
  (** If Some, a fade is in progress *)
}

let rec idle_state : unit AmbientAgentState.blueprint =
  {
    state_functions = {
      AmbientAgentState.empty_state_functions with
      create_handlers = Some (fun () ->
        [
          Channel.attach_handler Channels.Music.music (fun command _ ->
            let open Channels.Music in
            match command with
            | PlaySong filename ->
              (* Load and play new music *)
              let music = Raylib.load_music_stream filename in
              Raylib.play_music_stream music;
              let state = {
                current_music = music ;
                current_volume = 1.0 ;
                fade_state = None ;
              } in
              Some (AmbientAgentState.create playing_music_state state)
            | PerformFade _ ->
              failwith "tried to fade while no music was playing"
            | StopSong ->
              failwith "tried to stop song while no music was playing"
          )
        ]
      ) ;
    } ;
    props = {
      name = "Idle" ;
    }
  }

and playing_music_state : playing_music_fields AmbientAgentState.blueprint =
  {
    state_functions = {
      AmbientAgentState.empty_state_functions with
      script = Some (fun (board : board_interface) (state : playing_music_fields) ->
        let open AmbientAgentState in
        while true do
          (* Update the current music stream *)
          Raylib.update_music_stream state.current_music;

          (* Update fade state if fading *)
          (match state.fade_state with
          | Some fade ->
            let elapsed = Raylib.get_time () -. fade.start_time in
            if elapsed < fade.duration then begin
              let progress = elapsed /. fade.duration in
              let volume = fade.start_volume +. (fade.end_volume -. fade.start_volume) *. progress in
              state.current_volume <- volume;
              Raylib.set_music_volume state.current_music volume
            end else begin
              (* Fade complete *)
              state.current_volume <- fade.end_volume;
              fade.is_finished := true;
              Raylib.set_music_volume state.current_music fade.end_volume;
              state.fade_state <- None
            end
          | None -> ());

          Effect.perform (AmbientAction ())
        done
      ) ;
      create_handlers = Some (fun (state : playing_music_fields) ->
        [
          Channel.attach_handler Channels.Music.music (fun command _ ->
            let open Channels.Music in
            match command with
            | PlaySong filename ->
              (* Stop and unload current music *)
              Raylib.stop_music_stream state.current_music;
              Raylib.unload_music_stream state.current_music;
              (* Load and play new music *)
              let music = Raylib.load_music_stream filename in
              Raylib.play_music_stream music;
              let new_state = {
                current_music = music ;
                current_volume = 1.0 ;
                fade_state = None ;
              } in
              Some (AmbientAgentState.create playing_music_state new_state)
            | PerformFade (start_volume, end_volume, duration, is_finished) ->
              state.fade_state <- Some {
                start_volume ;
                end_volume ;
                duration ;
                is_finished ;
                start_time = Raylib.get_time ()
              };
              None
            | StopSong ->
              Raylib.stop_music_stream state.current_music;
              Raylib.unload_music_stream state.current_music;
              Some (AmbientAgentState.create idle_state ())
          )
        ]
      ) ;
    } ;
    props = {
      name = "PlayingMusic" ;
    }
  }

let maestro_class : ambient_agent_class =
  {
    states = StringMap.empty
      |> StringMap.add "Idle" idle_state.props
      |> StringMap.add "PlayingMusic" playing_music_state.props ;
    initial_state = AmbientAgentState.create idle_state () ;
    name = "Maestro" ;
  }
