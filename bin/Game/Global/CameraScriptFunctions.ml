open Common
open BoardInterface
open CameraStateCreators.AcquiredCameraState

let pan_along_path
    ?(before : pre_position option)
    ?(after : pre_position option)
    (path_name : string)
    (speed : float)
    (board : board_interface)
    (fields : camera_fields) : unit =
  (** [pan_along_path ?before ?after path_name speed board fields] smoothly pans the camera along the path named [path_name]
      at constant speed using arc-length parameterized cubic spline interpolation.
      [speed] is in pixels per second.
      [before] if provided, prepends an additional waypoint before the path.
      [after] if provided, appends an additional waypoint after the path.
      The spline interpolates through the extended sequence of waypoints. *)

  let open Raylib in
  let path = board.get_named_path path_name in

  (* Build the full node sequence with optional before/after points *)
  let full_nodes =
    let nodes = path.nodes in
    let nodes = match before with Some p -> p :: nodes | None -> nodes in
    let nodes = match after with Some p -> nodes @ [p] | None -> nodes in
    nodes
  in

  match full_nodes with
  | [] -> () (* Empty path, do nothing *)
  | [_single_node] -> () (* Single node path, do nothing *)
  | _ ->
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

    (* Create arc-length parameterized spline interpolation *)
    let num_nodes = List.length full_nodes in

    (* Convert nodes to camera positions *)
    let camera_positions = List.map cell_to_camera_center full_nodes in

    (* Extract x and y coordinates into arrays *)
    let x_coords = Array.of_list (List.map Vector2.x camera_positions) in
    let y_coords = Array.of_list (List.map Vector2.y camera_positions) in

    (* Create initial time parameter (uniform spacing) *)
    let t_params = Array.init num_nodes (fun i -> Float.of_int i) in

    (* Create cubic spline interpolations for x(t) and y(t) *)
    let x_spline = CubicSpline.create t_params x_coords in
    let y_spline = CubicSpline.create t_params y_coords in

    (* Calculate arc length by sampling the spline *)
    let num_samples = 1000 in
    let t_max = Float.of_int (num_nodes - 1) in

    (* Build arc-length table by numerical integration *)
    let arc_lengths = Array.make (num_samples + 1) 0.0 in
    for i = 1 to num_samples do
      let t_prev = t_max *. Float.of_int (i - 1) /. Float.of_int num_samples in
      let t_curr = t_max *. Float.of_int i /. Float.of_int num_samples in
      let dt = t_curr -. t_prev in

      (* Approximate derivatives at midpoint *)
      let t_mid = (t_prev +. t_curr) /. 2.0 in
      let dx_dt = CubicSpline.eval_deriv x_spline t_mid in
      let dy_dt = CubicSpline.eval_deriv y_spline t_mid in

      (* Arc length element: sqrt(dx² + dy²) * dt *)
      let ds = sqrt (dx_dt *. dx_dt +. dy_dt *. dy_dt) *. dt in
      arc_lengths.(i) <- arc_lengths.(i - 1) +. ds
    done;

    let total_arc_length = arc_lengths.(num_samples) in

    (* Helper: given arc length s, find corresponding parameter t using binary search *)
    let arc_length_to_t (s : float) : float =
      if s <= 0.0 then 0.0
      else if s >= total_arc_length then t_max
      else
        (* Binary search in arc_lengths array *)
        let rec binary_search lo hi =
          if hi - lo <= 1 then
            (* Linear interpolation between lo and hi *)
            let s_lo = arc_lengths.(lo) in
            let s_hi = arc_lengths.(hi) in
            let t_lo = t_max *. Float.of_int lo /. Float.of_int num_samples in
            let t_hi = t_max *. Float.of_int hi /. Float.of_int num_samples in
            let alpha = (s -. s_lo) /. (s_hi -. s_lo) in
            t_lo +. alpha *. (t_hi -. t_lo)
          else
            let mid = (lo + hi) / 2 in
            if arc_lengths.(mid) < s then
              binary_search mid hi
            else
              binary_search lo mid
        in
        binary_search 0 num_samples
    in

    (* Animate along the path at constant speed *)
    let total_duration = total_arc_length /. speed in
    let start_time = get_time () in

    let rec animate () =
      let elapsed = get_time () -. start_time in
      if elapsed >= total_duration then
        (* Ensure we end exactly at the last position *)
        let final_pos = vec2 (CubicSpline.eval x_spline t_max) (CubicSpline.eval y_spline t_max) in
        fields.pos <- clamp_camera final_pos
      else begin
        (* Calculate current arc length based on constant speed *)
        let current_arc_length = speed *. elapsed in
        (* Convert to parameter t *)
        let t = arc_length_to_t current_arc_length in
        (* Evaluate splines at t *)
        let x = CubicSpline.eval x_spline t in
        let y = CubicSpline.eval y_spline t in
        fields.pos <- clamp_camera (vec2 x y);
        ignore (Effect.perform @@ CameraAgentState.CameraAction ());
        animate ()
      end
    in
    animate ()
