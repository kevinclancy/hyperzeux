open Common
open LineBundleMap

type resource_bundle_def = {
  name : string ;
  texture_paths : string list ;
  ascii_font_paths : string list ;
  font_paths : string list ;
  agent_classes : Agent.agent_class list ;
  ambient_agent_classes : AmbientAgent.ambient_agent_class list ;
  region_agent_classes : RegionAgent.region_agent_class list ;
  camera_classes : CameraAgent.camera_agent_class list ;
  static_object_defs : static_object list ;
  line_drawing_bundles : LineBundleMap.line_drawing_bundle list;
}

include OrdMap.Make(
  struct
    type a = resource_bundle_def
    let get_name (x : resource_bundle_def) : string =
      x.name
  end
)

let load_bundle (name : string) : unit =
  (** Load the bundle with the given name.
      Paths in the bundle are relative to ./images/[name]/
      and will be automatically prefixed to create keys like [name]/[relative_path] *)

  let bundle = get name in
  let prefix_path path = name ^ "/" ^ path in

  (* Load textures *)
  List.iter (fun path ->
    TextureMap.load (prefix_path path)
  ) bundle.texture_paths;

  (* Load ASCII fonts *)
  List.iter (fun path ->
    let font_name =
      (prefix_path path)
      |> String.split_on_char '/'
      |> List.filter (fun x -> x <> "ascii_fonts")
      |> String.concat "/"
      |> Filename.remove_extension
    in
    TextureMap.load_ascii_font (prefix_path path) font_name;
    StaticObjectMap.add_ascii_font font_name
  ) bundle.ascii_font_paths;

  (* Load fonts *)
  List.iter (fun path ->
    FontMap.load (prefix_path path)
  ) bundle.font_paths;

  List.iter AgentClassMap.add bundle.agent_classes;
  List.iter AmbientAgentClassMap.add bundle.ambient_agent_classes;
  List.iter RegionAgentClassMap.add bundle.region_agent_classes;
  List.iter CameraAgentClassMap.add bundle.camera_classes;
  List.iter StaticObjectMap.add bundle.static_object_defs;
  List.iter LineBundleMap.add bundle.line_drawing_bundles
