open Common

(** [trim_zeros txt] Returns [txt] with all of the zero bytes trimmed off of the end *)
let trim_zeros (txt : string) : string =
  if String.length txt > 0 && ((Char.code txt.[String.length txt - 1]) = 0) then
    String.sub txt 0 (String.length txt - 1)
  else
    txt

let get_input_string (description : string) : string option =
(** [get_input_string description]

Displays an interactive textbox which receives a string from user input.

## Parameters

* [description] - A label above the textbox, describing the string requested from the user

## Returns

* Some(str) where [str] is the user's input string if users exits with the Enter key

* None if the user exits with the Escape key

*)
  let open Raylib in
  let edit_text = ref "" in
  let choice : string option ref = ref None in

  begin_drawing ();
    (* Intentionally left empty to clear out input data,
       i.e. prevent the textbox from starting out containing the letter of the key
       that opened the dialog *)
  end_drawing ();

  while (not @@ window_should_close ()) && (Option.is_none !choice) do
    clear_background Color.gray;
    begin_drawing ();
      Raygui.label (Rectangle.create 10.0 10.0 300.0 14.0) description;
      edit_text :=
      begin
        Raygui.set_style (Default `Text_size) 24;
        let txt,_ = Raygui.text_box (rect 10.0 30.0 300.0 80.0) !edit_text true in
        trim_zeros txt;
      end;
      if (is_key_pressed Key.Enter) then
          choice := Some !edit_text;
    end_drawing ()
  done;
  !choice

let get_new_name (label : string) (is_forbidden : string -> bool) : string option =
(** [get_new_name is_forbidden]

Displays an interactive textbox which receives a string from user input.

## Parameters

* [label] - The label to display the textbox with

* [is_forbidden] - A predicate that returns `true` if the argument is forbidden as a selection
                   (perhaps because it's already being "used")

## Returns

* Some(str) where [str] is the user's input string if [str] is not forbidden and the user exits with the Enter key

* None if the user exits with the Escape key

*)
  let open Raylib in
  let edit_text = ref "" in
  let choice : string option ref = ref None in
  while (not @@ window_should_close ()) && (Option.is_none !choice) do
    clear_background Color.gray;
    begin_drawing ();
      Raygui.label (Rectangle.create 10.0 10.0 300.0 14.0) label;
      edit_text :=
        begin
          Raygui.set_style (Default `Text_size) 24;
          if is_forbidden !edit_text then
              Raylib.draw_text "name already used!" 10 120 24 Raylib.Color.red;
          let txt,_ = Raygui.text_box (rect 10.0 30.0 300.0 80.0) !edit_text true in
          trim_zeros txt
        end;
      if (is_key_pressed Key.Enter) && (not @@ is_forbidden !edit_text) && (not @@ String.equal !edit_text "") then
        choice := Some !edit_text;
    end_drawing ()
  done;
  !choice

(** [get_item search get_name get_texture]

Returns a user-selected item, where
[search text] produces a list of items whose names match [text],
[get_name item] gets the name of [item], and [get_texture item], when present, gets the texture of [item].
*)
let get_item (search : string -> 'a list)
             ?(get_texture : ('a -> Raylib.Texture.t) option)
             (get_name : 'a -> string) : 'a option =
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
        let (but_x, but_y) = (10.0, (10.0 +. (m +. 1.0) *. 50.0)) in
        let (but_w, but_h) = (300.0, 50.0) in
        let button_rect = rect but_x but_y but_w but_h in
        begin
          match !selected_index with
          | Some m when m = n ->
            Raygui.set_style (Button `Base_color_normal) 0xff00ffff
          | _ ->
            Raygui.set_style (Button `Base_color_normal) 0x0f0f0fff
          end;
          if Raygui.button button_rect (get_name obj) then
            selection := Some obj;
          match get_texture with
          | Some(f) ->
            begin
              let texture = f obj in
              let tex_width = Float.of_int @@ Texture.width texture in
              let tex_height = Float.of_int @@ Texture.height texture in
              let dest_tex_rect =
                rect
                  (but_x +. but_w -. (2.0 *. tex_width) -. 10.0)
                  (but_y +. 10.0)
                  (2.0 *. tex_width)
                  (2.0 *. tex_height)
              in
              Raylib.draw_texture_pro
                  texture
                  (rect 0.0 0.0 tex_width tex_height)
                  dest_tex_rect
                  (Vector2.zero ())
                  0.0
                  Color.white;
          end
        | None ->
          ()
      ) !obj_list;
    end_drawing ();
  done;
  !selection