let char_width = 8
let char_height = 14

let board_cells_width = 30
let board_cells_height = 20

let board_pixels_width = char_width * board_cells_width
let board_pixels_height = char_height * board_cells_height

let screen_width = 2048
let screen_height = 1536
let screen_width_f = Float.of_int screen_width
let screen_height_f = Float.of_int screen_height

let object_selector_width = 300.0
let object_selector_height = 280.0

(** Distance from object selector to window edge *)
let object_selector_margin = 10.0

let agent_selector_width = 350.0
let agent_selector_height = 280.0
(** Distance from agent selector to window edge *)
let agent_selector_margin = 10.0

let editor_zoom_speed = 1.0
let editor_pan_speed = 10.0

let grid_color = Raylib.Color.create 0 255 0 80

let speed = 10.0