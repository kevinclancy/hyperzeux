open Common

include OrdMap.Make(
  struct
    type a = static_object
    let get_name (x : static_object) : string =
      x.name
  end
)

let add_ascii_font (font_prefix : string) =
  for char_code = 32 to 127 do
    let name = String.concat "" [font_prefix; Int.to_string char_code] in
    add {
      texture_name = name;
      traversable = false ;
      name = name
    };
  done