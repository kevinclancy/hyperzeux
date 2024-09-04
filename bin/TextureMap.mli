
(** [load filename] Loads a texture from [filename], where [filename] is the path of an image
    relative to the images subdirectory. Stores the texture in the texture map with key [filename].
*)
val load : string -> unit

(** [get name] Gets the texture with key [name] from the texture map *)
val get : string -> Raylib.Texture.t