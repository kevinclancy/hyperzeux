open Common

(** [trim_zeros txt] Returns [txt] with all of the zero bytes trimmed off of the end *)
let trim_zeros (txt : string) : string =
  if String.length txt > 0 && ((Char.code txt.[String.length txt - 1]) = 0) then
    String.sub txt 0 (String.length txt - 1)
  else
    txt

(** [get_item search get_name get_texture] Returns a user-selected item, where
[search text] produces a list of items whose names match [text],
[get_name item] gets the name of [item], and [get_texture item] gets the texture of [item].
*)
let get_item (search : string -> 'a list)
             (get_name : 'a -> string)
             (get_texture : 'a -> Raylib.Texture.t) : 'a option =
  let open Raylib in

  begin_drawing ();
    (* Intentionally left empty to clear out input data,
       i.e. prevent the textbox from starting out containing the letter of the key
       that opened the static object selector *)
  end_drawing ();

  let edit_text = ref "" in
  let obj_list = ref [] in
  let selection = ref None in
  let selected_index = ref None in
  while (not @@ window_should_close ()) && (Option.is_none !selection) do
    clear_background Color.gray;
    begin_drawing ();
      let txt, _ =
        Raygui.set_style (Default `Text_size) 24;
        Raygui.text_box (rect 10.0 10.0 300.0 50.0) !edit_text true
      in
      let txt = trim_zeros txt in
      if not @@ String.equal !edit_text txt then
        begin
          edit_text := txt;
          obj_list := search txt;
          if List.length (!obj_list) > 0 then
            selected_index := Some 0
          else
            selected_index := None
        end;
      if is_key_pressed Key.Down then
        begin
        match !selected_index with
        | Some m ->
          selected_index := Some ((m + 1) mod (List.length !obj_list))
        | None ->
          ()
        end;
      if is_key_pressed Key.Up then
        begin
          match !selected_index with
          | Some m  ->
            selected_index := Some (if m = 0 then (List.length !obj_list - 1) else m - 1)
          | None ->
            ()
        end;
      if is_key_pressed Key.Enter && Option.is_some !selected_index then
        begin
          selection := Some (List.nth !obj_list (Option.get !selected_index))
        end;
      List.iteri (fun n obj ->
        let m = Float.of_int @@ n in
        let texture = get_texture obj in
        let (but_x, but_y) = (10.0, (10.0 +. (m +. 1.0) *. 50.0)) in
        let (but_w, but_h) = (300.0, 50.0) in
        let button_rect = rect but_x but_y but_w but_h in
        let tex_width = Float.of_int @@ Texture.width texture in
        let tex_height = Float.of_int @@ Texture.height texture in
        let dest_tex_rect =
          rect
            (but_x +. but_w -. (2.0 *. tex_width) -. 10.0)
            (but_y +. 10.0)
            (2.0 *. tex_width)
            (2.0 *. tex_height)
        in
        begin
        match !selected_index with
        | Some m when m = n ->
          Raygui.set_style (Button `Base_color_normal) 0xff00ffff
        | _ ->
          Raygui.set_style (Button `Base_color_normal) 0x0f0f0fff
        end;
        if Raygui.button button_rect (get_name obj) then
          selection := Some obj;
        Raylib.draw_texture_pro
          texture
          (rect 0.0 0.0 tex_width tex_height)
          dest_tex_rect
          (Vector2.zero ())
          0.0
          Color.white;
      ) !obj_list;
    end_drawing ();
  done;
  !selection