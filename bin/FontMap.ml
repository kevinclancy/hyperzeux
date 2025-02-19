open Common

let font_map : Raylib.Font.t StringMap.t ref = ref StringMap.empty

let load (filename : string) =
  (* ick... use string interpolation instead *)
  let full_filename = String.concat "" ["./fonts/" ; filename] in
  let font_map' =
    StringMap.add
      filename
      (full_filename |> Raylib.load_font)
      !font_map
  in
  font_map := font_map'

  let get (name : string) =
    StringMap.find name !font_map

