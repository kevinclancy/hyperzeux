open Common

type resource_bundle_def = {
  name : string ;
  texture_paths : string list ;
  ascii_font_path : string ;
  font_paths : string list ;
  agent_classes : Agent.agent_class list ;
  ambient_agent_classes : AmbientAgent.ambient_agent_class list ;
  camera_class : CameraAgent.camera_agent_class ;
  static_object_defs : static_object list ;
}

include OrdMap.Make(
  struct
    type a = resource_bundle_def
    let get_name (x : resource_bundle_def) : string =
      x.name
  end
)

let load_bundle (name : string) : unit =
  (** Load the bundle with the given name *)

  let bundle = get name in
  List.iter TextureMap.load bundle.texture_paths;

  TextureMap.load_ascii_font bundle.ascii_font_path "ascii";
  StaticObjectMap.add_ascii_font "ascii";

  List.iter FontMap.load bundle.font_paths;
  List.iter AgentClassMap.add bundle.agent_classes;
  List.iter AmbientAgentClassMap.add bundle.ambient_agent_classes;
  CurrentCameraClass.set bundle.camera_class;
  List.iter StaticObjectMap.add bundle.static_object_defs
