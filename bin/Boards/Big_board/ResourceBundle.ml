
let resources : ResourceBundleMap.resource_bundle_def = {
  name = "big_board" ;
  texture_paths = [
    "person_south_recon.png";
    "person_north_recon.png";
    "person_east_recon.png";
    "person_west_recon.png";
    "person2_south.png";
    "person2_north.png";
    "person2_east.png";
    "person2_west.png";
    "empty_cell.png";
    "solid_wall.png";
    "checkered_wall.png";
    "plant_1.png";
    "plant_2.png";
    "waypoint.png";
  ] ;
  font_paths = [
    "romulus.png"
  ] ;
  agent_classes = [
    Agents.Button.button_class;
    Agents.Patroller.patroller_class;
    Shared.Agents.Player.player_class
  ] ;
  ambient_agent_classes = [
    Shared.Ambient_agents.SpeechBox.speech_box_class
  ] ;
  camera_class = Camera.camera ;
  static_object_defs = [
    { name = "empty" ; texture_name = "empty_cell.png" ; traversable = true } ;
    { name = "wall" ; texture_name = "solid_wall.png" ; traversable = false } ;
    { name = "checkered_wall" ; texture_name = "checkered_wall.png" ; traversable = false } ;
    { name = "plant_1" ; texture_name = "plant_1.png" ; traversable = true } ;
    { name = "plant_2" ; texture_name = "plant_2.png" ; traversable = true }
  ]
}