open Common
open Region

let draw_text_in_region (set_static_object : position -> string -> unit) (refresh_static : region -> unit) (regions : region StringMap.t) (region_name : string) (text_to_draw : string) : unit =
  (** [draw_text_in_region set_static_object refresh_static regions region_name text_to_draw]
      Draws [text_to_draw] into the region named [region_name],
      starting from the top-left corner of the region and wrapping text as needed.
      [set_static_object pos static_obj_name] sets the static object at position [pos] to the static object named [static_obj_name].
      [refresh_static region] refreshes the static texture of the layer containing the region, but only within the area of the region for efficiency.
      Precondition: the region must have exactly one component. *)

  let region = StringMap.find region_name regions in

  assert (StringMap.cardinal region.components = 1);

  let _, component = StringMap.choose region.components in

  let start_x = component.left in
  let start_y = component.top in
  let right_boundary = component.right in
  let bottom_boundary = component.bottom in
  let layer_name = component.layer in

  let current_x = ref start_x in
  let current_y = ref start_y in

  String.iter
    (fun c ->
      let char_code = Char.code c in

      if char_code = 10 then begin
        current_x := start_x;
        current_y := !current_y + 1
      end
      else if !current_y <= bottom_boundary then begin
        let char_obj_name = String.concat "" ["ascii" ; Int.to_string char_code] in
        set_static_object {layer = layer_name; x = !current_x; y = !current_y} char_obj_name;

        current_x := !current_x + 1;
        if !current_x > right_boundary then begin
          current_x := start_x;
          current_y := !current_y + 1
        end
      end
    )
    text_to_draw;

  refresh_static region
