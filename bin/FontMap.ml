open Common

let font_map : Raylib.Font.t StringMap.t ref = ref StringMap.empty

let load (relative_path : string) =
  let full_path = "./images/" ^ relative_path in
  let font_map' =
    StringMap.add
      relative_path
      (full_path |> Raylib.load_font)
      !font_map
  in
  font_map := font_map'

let get (name : string) =
  match StringMap.find_opt name !font_map with
  | Some font ->
    font
  | None ->
    Printf.printf "Could not find font %s\n" name;
    raise Not_found
