open Common

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
    "double_line_nesw.png" ;
    "double_line_nesx.png" ;
    "double_line_nexw.png" ;
    "double_line_nexx.png" ;
    "double_line_nxsw.png" ;
    "double_line_nxsx.png" ;
    "double_line_nxxw.png" ;
    "double_line_xesw.png" ;
    "double_line_xesx.png" ;
    "double_line_xexw.png" ;
    "double_line_xxsw.png" ;
    "double_line_xxxx.png"
  ] ;
  ascii_font_path = "charset.png" ;
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
    { name = "plant_2" ; texture_name = "plant_2.png" ; traversable = true } ;

    (** Todo: create a system to auto-define line bundles using a prefix such as 'double_line' *)
    { name = "double_line_nesw" ; texture_name = "double_line_nesw.png" ; traversable = false } ;
    { name = "double_line_nesx" ; texture_name = "double_line_nesx.png" ; traversable = false } ;
    { name = "double_line_nexw" ; texture_name = "double_line_nexw.png" ; traversable = false } ;
    { name = "double_line_nexx" ; texture_name = "double_line_nexx.png" ; traversable = false } ;
    { name = "double_line_nxsw" ; texture_name = "double_line_nxsw.png" ; traversable = false } ;
    { name = "double_line_nxsx" ; texture_name = "double_line_nxsx.png" ; traversable = false } ;
    { name = "double_line_nxxw" ; texture_name = "double_line_nxxw.png" ; traversable = false } ;
    { name = "double_line_xesw" ; texture_name = "double_line_xesw.png" ; traversable = false } ;
    { name = "double_line_xesx" ; texture_name = "double_line_xesx.png" ; traversable = false } ;
    { name = "double_line_xexw" ; texture_name = "double_line_xexw.png" ; traversable = false } ;
    { name = "double_line_xxsw" ; texture_name = "double_line_xxsw.png" ; traversable = false } ;
    { name = "double_line_xxxx" ; texture_name = "double_line_xxxx.png" ; traversable = false } ;
  ];

  line_drawing_bundles =
    let open ResourceBundleMap in
    [
      {
        name = "double_line" ;
        nesw = "double_line_nesw" ;
        nesx = "double_line_nesx" ;
        nexw = "double_line_nexw" ;
        nexx = "double_line_nexx" ;
        nxws = "double_line_nxsw" ;
        nxsx = "double_line_nxsx" ;
        nxxw = "double_line_nxxw" ;
        xesw = "double_line_xesw" ;
        xesx = "double_line_xesx" ;
        xexw = "double_line_xexw" ;
        xxsw = "double_line_xxsw" ;
        xxxx = "double_line_xxxx" ;
      }
    ]
}