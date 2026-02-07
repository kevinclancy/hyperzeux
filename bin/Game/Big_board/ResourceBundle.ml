open Common

let resources : ResourceBundleMap.resource_bundle_def = {
  name = "big_board" ;
  texture_paths = [
    "characters/ruffian.png";
  ] ;
  ascii_font_paths = [] ;
  font_paths = [] ;
  agent_classes = [
    Agents.Patroller.patroller_class;
  ] ;
  ambient_agent_classes = [] ;
  region_agent_classes = [
    Agents.HotSpot.hotspot_class
  ];
  camera_classes = [] ;
  static_object_defs = [];
  line_drawing_bundles = [];
}