open Common

let texture_map : Raylib.Texture.t StringMap.t ref = ref StringMap.empty

let load_ascii_font (image_path : string) (font_prefix : string) : unit =
  let open Raylib in
  (* ick... use string interpolation instead *)
  let full_filename = String.concat "" ["./images/" ; image_path] in
  let charset = Raylib.load_texture_from_image (load_image full_filename) in
  let code = ref 32 in
  for row = 0 to 2 do
    for col = 0 to 31 do
      let char_render_texture = Raylib.load_render_texture 8 14 in
      begin_texture_mode char_render_texture;
        let top, left = (Float.of_int @@ row * 14), (Float.of_int @@ col * 8) in
        draw_texture_rec charset (Rectangle.create left top 8. (-14.)) (vec2 0. 0.) Color.white;
      end_texture_mode ();
      let texname = String.concat "" [font_prefix ; Int.to_string !code] in
      texture_map := StringMap.add texname (RenderTexture.texture char_render_texture) !texture_map;
      code := !code + 1;
    done;
  done

let load (filename : string) : unit =
  (* ick... use string interpolation instead *)
  let full_filename = String.concat "" ["./images/" ; filename] in
  let texture_map' =
    StringMap.add
      filename
      (full_filename |> Raylib.load_image |> Raylib.load_texture_from_image)
      !texture_map
  in
  texture_map := texture_map'

  let get (name : string) : Raylib.Texture.t =
    StringMap.find name !texture_map

