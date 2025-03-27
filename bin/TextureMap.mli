
val load : string -> unit
(** [load filename] Loads a texture from [filename], where [filename] is the path of an image
    relative to the images subdirectory. Stores the texture in the texture map with key [filename].
*)

val load_ascii_font : string -> string -> unit
(** [load_ascii_font image_path font_prefix]

    Loads a partial ascii character set containing alphanumeric and punctuation characters
    Each character is 8x14 pixels. Each row contains 38 characters. The first row
    contains characters 32 (space) through 63 (question mark). The next row contains
    characters '@' through '_'. The third and final row contains characters '`' through 'delete'.

    These characters get added to the texture map individually as '[font_prefix]32' through '[font_prefix]127'
    *)

val get : string -> Raylib.Texture.t
(** [get name] Gets the texture with key [name] from the texture map *)