
let () =
  let agent = Patroller.create (3,3) in
  let _ = Agent.resume agent in
  Raylib.init_window Config.board_pixels_width Config.board_pixels_height "raylib [core] example - basic window";
  Raylib.set_target_fps 60;
  (* let texture_map : Raylib.Texture.t StringMap.t ref = ref StringMap.empty in
  texture_map := load_texture !texture_map "person_forward_recon.png"; *)
  TextureMap.load_texture "person_forward_recon.png";
  while not (Raylib.window_should_close ()) do
    let open Raylib in
    begin_drawing ();
    clear_background Color.black;
    draw_text "Congrats! You created your first window!" 190 200 20
      Color.lightgray;
    (* draw_texture patroller_texture 50 50 Color.lime; *)
    end_drawing ();
  done;

  Raylib.close_window ();
