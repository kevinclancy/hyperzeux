open Common

let resources : ResourceBundleMap.resource_bundle_def = {
  name = "global" ;
  texture_paths = [
    "characters/person_south_recon.png";
    "characters/person_north_recon.png";
    "characters/person_east_recon.png";
    "characters/person_west_recon.png";
    "characters/woman1_north.png";
    "characters/woman1_east.png";
    "characters/woman1_south.png";
    "characters/woman1_west.png";
    "characters/person2_south.png";
    "characters/person2_north.png";
    "characters/person2_east.png";
    "characters/person2_west.png";
    "scene/empty_cell.png";
    "scene/solid_wall.png";
    "scene/transparent.png";
    "scene/transparent_viz.png";
    "scene/checkered_wall.png";
    "scene/plant_1.png";
    "scene/plant_2.png";
    "scene/waypoint.png";
    "line_styles/double_line/nesw.png" ;
    "line_styles/double_line/nesx.png" ;
    "line_styles/double_line/nexw.png" ;
    "line_styles/double_line/nexx.png" ;
    "line_styles/double_line/nxsw.png" ;
    "line_styles/double_line/nxsx.png" ;
    "line_styles/double_line/nxxw.png" ;
    "line_styles/double_line/xesw.png" ;
    "line_styles/double_line/xesx.png" ;
    "line_styles/double_line/xexw.png" ;
    "line_styles/double_line/xxsw.png" ;
    "line_styles/double_line/xxxx.png"
  ] ;
  ascii_font_paths = [
    "ascii_fonts/ascii.png"
  ] ;
  font_paths = [
    "fonts/romulus.png"
  ] ;
  agent_classes = [
    Agents.Player.player_class
  ] ;
  ambient_agent_classes = [
    AmbientAgents.Maestro.maestro_class
  ] ;
  region_agent_classes = [] ;
  camera_classes = [
    Cameras.WorldCamera.camera;
    Cameras.InterfaceCamera.camera
  ] ;
  static_object_defs = [
    { name = "empty" ; texture_name = "global/scene/empty_cell.png" ; traversable = true } ;
    { name = "wall" ; texture_name = "global/scene/solid_wall.png" ; traversable = false } ;
    { name = "checkered_wall" ; texture_name = "global/scene/checkered_wall.png" ; traversable = false } ;
    { name = "plant_1" ; texture_name = "global/scene/plant_1.png" ; traversable = true } ;
    { name = "plant_2" ; texture_name = "global/scene/plant_2.png" ; traversable = true } ;
    { name = "transparent" ; texture_name = "global/scene/transparent.png" ; traversable = false } ;
    { name = "double_line_nesw" ; texture_name = "global/line_styles/double_line/nesw.png" ; traversable = false } ;
    { name = "double_line_nesx" ; texture_name = "global/line_styles/double_line/nesx.png" ; traversable = false } ;
    { name = "double_line_nexw" ; texture_name = "global/line_styles/double_line/nexw.png" ; traversable = false } ;
    { name = "double_line_nexx" ; texture_name = "global/line_styles/double_line/nexx.png" ; traversable = false } ;
    { name = "double_line_nxsw" ; texture_name = "global/line_styles/double_line/nxsw.png" ; traversable = false } ;
    { name = "double_line_nxsx" ; texture_name = "global/line_styles/double_line/nxsx.png" ; traversable = false } ;
    { name = "double_line_nxxw" ; texture_name = "global/line_styles/double_line/nxxw.png" ; traversable = false } ;
    { name = "double_line_xesw" ; texture_name = "global/line_styles/double_line/xesw.png" ; traversable = false } ;
    { name = "double_line_xesx" ; texture_name = "global/line_styles/double_line/xesx.png" ; traversable = false } ;
    { name = "double_line_xexw" ; texture_name = "global/line_styles/double_line/xexw.png" ; traversable = false } ;
    { name = "double_line_xxsw" ; texture_name = "global/line_styles/double_line/xxsw.png" ; traversable = false } ;
    { name = "double_line_xxxx" ; texture_name = "global/line_styles/double_line/xxxx.png" ; traversable = false } ;
  ];

  line_drawing_bundles = [
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
  ];
}
