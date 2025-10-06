open Common
open BoardInterface
open CameraStateCreators.AcquiredCameraState

let pan_along_path (path_name : string) (board : board_interface) (fields : camera_fields) : unit =
  (** [pan_along_path path_name board fields] smoothly pans the camera along the path named [path_name].
      It first pans from the current position to the first node in the path,
      then follows the path using bezier curves between consecutive nodes,
      and finally pans back to the original camera position. *)

  let open Raylib in
  let path = board.get_named_path path_name in

  match path.nodes with
  | [] -> () (* Empty path, do nothing *)
  | first_node :: _ ->
    (* Record the starting camera position to return to *)
    let original_camera_pos = fields.pos in

    (* Helper to convert cell position to camera position centered on that cell *)
    let cell_to_camera_center (cell_pos : pre_position) : vec2 =
      let viewport_width = Config.screen_width_f /. fields.scale in
      let viewport_height = Config.screen_height_f /. fields.scale in
      let camera_offset = vec2 (viewport_width /. 2.) (viewport_height /. 2.) in
      (* Convert pre_position to pixel coordinates *)
      let cell_pixel_x = Float.of_int cell_pos.x *. Common.char_width_f +. (Common.char_width_f /. 2.0) in
      let cell_pixel_y = Float.of_int cell_pos.y *. Common.char_height_f +. (Common.char_height_f /. 2.0) in
      let cell_center = Vector2.create cell_pixel_x cell_pixel_y in
      cell_center ^- camera_offset
    in

    (* Clamp camera position to board bounds *)
    let clamp_camera (pos : vec2) : vec2 =
      let viewport_width = Config.screen_width_f /. fields.scale in
      let viewport_height = Config.screen_height_f /. fields.scale in
      let max_camera_x = Config.board_pixels_width_f -. viewport_width in
      let max_camera_y = Config.board_pixels_height_f -. viewport_height in
      vec2
        (min (max (Vector2.x pos) 0.) max_camera_x)
        (min (max (Vector2.y pos) 0.) max_camera_y)
    in

    (* Pan from start to end over duration seconds *)
    let pan_to (end_pos : vec2) (duration : float) : unit =
      let start_pos = fields.pos in
      let start_time = get_time () in
      let rec loop () =
        let elapsed = get_time () -. start_time in
        if elapsed >= duration then
          fields.pos <- clamp_camera end_pos
        else begin
          let t = elapsed /. duration in
          let interpolated = Vector2.lerp start_pos end_pos t in
          fields.pos <- clamp_camera interpolated;
          ignore (Effect.perform @@ CameraAgentState.CameraAction ());
          loop ()
        end
      in
      loop ()
    in

    (* Pan to first node *)
    let first_camera_pos = cell_to_camera_center first_node in
    pan_to first_camera_pos 2.0;

    (* Pan along path with smooth interpolation between nodes *)
    let rec follow_path nodes =
      match nodes with
      | p1 :: p2 :: rest ->
        let start_pos = cell_to_camera_center p1 in
        let end_pos = cell_to_camera_center p2 in
        let duration = 2.0 in
        let start_time = get_time () in
        let rec loop () =
          let elapsed = get_time () -. start_time in
          if elapsed >= duration then begin
            fields.pos <- clamp_camera end_pos;
            ignore (Effect.perform @@ CameraAgentState.CameraAction ())
          end else begin
            let t = elapsed /. duration in
            (* Linear interpolation between start and end *)
            let interpolated_pos = Vector2.lerp start_pos end_pos t in
            fields.pos <- clamp_camera interpolated_pos;
            ignore (Effect.perform @@ CameraAgentState.CameraAction ());
            loop ()
          end
        in
        loop ();
        follow_path (p2 :: rest)
      | _ -> ()
    in
    follow_path path.nodes;

    (* Pan back to original camera position *)
    pan_to original_camera_pos 2.0
