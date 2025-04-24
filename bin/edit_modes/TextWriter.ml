
open Common

type text_writer_state =
  | Writing of position
  (** User is typing text, where [pos] is the cell location of the next character to type *)
  | Locating
  (** Waiting for user to choose a cell to type in *)

type t = text_writer_state ref

let text_writer_width = 350.0
let text_writer_height = 680.0
(** Distance from agent selector to window edge *)

let text_writer_margin = 10.0

let create () : t = ref Locating

let non_alpha_assoc =
  let open Raylib in
  [
    (Key.Space, 32) ;
  ]

let upper_assoc =
  let open Raylib in
  [
    (Key.Space, 32) ;
    (Key.Grave, 126) ;

    (Key.One, 33) ;
    (Key.Two, 64) ;
    (Key.Three, 35) ;
    (Key.Four, 36) ;
    (Key.Five, 37) ;
    (Key.Six, 94) ;
    (Key.Seven, 38) ;
    (Key.Eight, 42) ;
    (Key.Nine, 40) ;
    (Key.Zero, 41) ;

    (Key.Minus, 95) ;
    (Key.Equal, 43) ;

    (Key.Left_bracket, 123) ;
    (Key.Right_bracket, 125) ;
    (Key.Backslash, 124) ;
    (Key.Semicolon, 58) ;
    (Key.Apostrophe, 34) ;
    (Key.Comma, 60) ;
    (Key.Period, 62) ;
    (Key.Slash, 63) ;

    (Key.A, 65) ;
    (Key.B, 66) ;
    (Key.C, 67) ;
    (Key.D, 68) ;
    (Key.E, 69) ;
    (Key.F, 70) ;
    (Key.G, 71) ;
    (Key.H, 72) ;
    (Key.I, 73) ;
    (Key.J, 74) ;
    (Key.K, 75) ;
    (Key.L, 76) ;
    (Key.M, 77) ;
    (Key.N, 78) ;
    (Key.O, 79) ;
    (Key.P, 80) ;
    (Key.Q, 81) ;
    (Key.R, 82) ;
    (Key.S, 83) ;
    (Key.T, 84) ;
    (Key.U, 85) ;
    (Key.V, 86) ;
    (Key.W, 87) ;
    (Key.X, 88) ;
    (Key.Y, 89) ;
    (Key.Z, 90) ;
  ]

  let lower_assoc =
    let open Raylib in
    [
      (Key.Space, 32) ;
      (Key.Grave, 96) ;

      (Key.One, 49) ;
      (Key.Two, 50) ;
      (Key.Three, 51) ;
      (Key.Four, 52) ;
      (Key.Five, 53) ;
      (Key.Six, 54) ;
      (Key.Seven, 55) ;
      (Key.Eight, 56) ;
      (Key.Nine, 57) ;

      (Key.Minus, 45) ;
      (Key.Equal, 61) ;

      (Key.Left_bracket, 91) ;
      (Key.Right_bracket, 93) ;
      (Key.Backslash, 92) ;
      (Key.Semicolon, 59) ;
      (Key.Apostrophe, 39) ;
      (Key.Comma, 44) ;
      (Key.Period, 46) ;
      (Key.Slash, 47) ;

      (Key.A, 97) ;
      (Key.B, 98) ;
      (Key.C, 99) ;
      (Key.D, 100) ;
      (Key.E, 101) ;
      (Key.F, 102) ;
      (Key.G, 103) ;
      (Key.H, 104) ;
      (Key.I, 105) ;
      (Key.J, 106) ;
      (Key.K, 107) ;
      (Key.L, 108) ;
      (Key.M, 109) ;
      (Key.N, 110) ;
      (Key.O, 111) ;
      (Key.P, 112) ;
      (Key.Q, 113) ;
      (Key.R, 114) ;
      (Key.S, 115) ;
      (Key.T, 116) ;
      (Key.U, 117) ;
      (Key.V, 118) ;
      (Key.W, 119) ;
      (Key.X, 120) ;
      (Key.Y, 121) ;
      (Key.Z, 122)
    ]

let draw (text_writer : t)
         (bp : Board.Blueprint.t)
         (camera_pos : vec2)
         (scale : float)
         (mouse_pos : vec2) : Raylib.Rectangle.t =

  let open Raylib in
  let boundary_left = Config.screen_width_f -. text_writer_width -. text_writer_margin in
  let boundary_top = text_writer_margin in
  let outer_boundary = Rectangle.create boundary_left boundary_top text_writer_width text_writer_height in
  draw_rectangle_rec outer_boundary Color.black;

  begin match !text_writer with
  | Writing(pos) ->
    let romulus_font = FontMap.get "romulus.png" in
    let text_pos = vec2 (boundary_left +. 5.) (boundary_top +. 5.) in
    draw_text_ex romulus_font "Type text.\nPress escape when finished." text_pos 18.0 1.0 Color.white

    (** TODO: highlight current position *)

  | Locating ->
    let romulus_font = FontMap.get "romulus.png" in
    let text_pos = vec2 (boundary_left +. 5.) (boundary_top +. 5.) in
    draw_text_ex romulus_font "Left-click a board cell to begin\nwriting text." text_pos 18.0 1.0 Color.white
  end;
  outer_boundary

let click_left (text_writer : t) (bp : Board.Blueprint.t) (cursor_cell_pos : position)
               (camera_pos : vec2) (scale : float) : unit =
  text_writer := (Writing cursor_cell_pos)

let write_char (text_writer : t) (char_code : int) (bp : Board.Blueprint.t) (pos : position) : unit =
  let char_obj_name = String.concat "" ["ascii" ; Int.to_string char_code] in
  Board.Blueprint.set_static_object bp pos char_obj_name Raylib.Color.white;
  if pos.x + 1 < Board.Blueprint.get_width bp then
    text_writer := Writing({ pos with x = pos.x + 1 })
  else
    text_writer := Locating

let handle_keypress (text_writer : t) (bp : Board.Blueprint.t) : bool =
  let open Raylib in
  match !text_writer with
  | Writing(pos) ->
    if (is_key_pressed Key.Escape) then
      begin
        text_writer := Locating;
        true
      end
    else if (is_key_down Key.Left_shift || is_key_down Key.Right_shift) then
      begin match List.find_opt (fun (key, _) -> is_key_pressed key) upper_assoc with
      | Some (_, code) ->
        write_char text_writer code bp pos;
        true
      | None ->
        false
      end
    else
      begin match List.find_opt (fun (key, _) -> is_key_pressed key) lower_assoc with
      | Some (_, code) ->
        write_char text_writer code bp pos;
        true
      | None ->
        false
      end
  | Locating ->
    false