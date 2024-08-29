module StringMap = Map.Make(String)

let texture_map : Raylib.Texture.t StringMap.t ref = ref StringMap.empty

let load_texture (filename : string) =
  (* ick... use string interpolation instead *)
  let full_filename = String.concat "" ["./images/" ; filename] in
  let texture_map' =
    StringMap.add
      filename
      (full_filename |> Raylib.load_image |> Raylib.load_texture_from_image)
      !texture_map
  in
  texture_map := texture_map'

  let get_texture (name : string) =
    StringMap.find name !texture_map

