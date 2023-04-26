let setup () =
  Raylib.init_window 800 450 "raylib [core] example - basic window";
  Raylib.set_target_fps 60

let change_rect rect x y =
  Raylib.Rectangle.set_x rect x;
  Raylib.Rectangle.set_y rect y

let within rect x1 y1 =
  let open Raylib.Rectangle in
  if
    x rect <= x1
    && x1 <= x rect +. width rect
    && y rect <= y1
    && y1 <= y rect +. height rect
  then true
  else false

let move_rect = Raylib.Rectangle.create 10. 100. 100. 40.
let stay_rect2 = Raylib.Rectangle.create 10. 100. 100. 40.
let turn_rect = Raylib.Rectangle.create 10. 150. 100. 40.
let stay_rect = Raylib.Rectangle.create 10. 150. 100. 40.
let start_button = Raylib.Rectangle.create 10. 200. 100. 40.
let stay_rect3 = Raylib.Rectangle.create 10. 200. 100. 40.
let end_button = Raylib.Rectangle.create 10. 250. 100. 40.
let stay_rect4 = Raylib.Rectangle.create 10. 200. 100. 40.
let on_screen = ref []
let default_x = 10
let default_y = 100
let defaul_spacing = 100
let block_selected = ref false
let block_selected2 = ref false

open Raylib

let update_x block =
  if
    is_mouse_button_down MouseButton.Left
    && (not !block_selected)
    && within block
         (float_of_int (get_mouse_x ()))
         (float_of_int (get_mouse_y ()))
  then
    let _ = block_selected := true in
    get_mouse_x () - 50
  else
    let _ = block_selected := false in
    int_of_float (Rectangle.x block)

let update_y block =
  if
    is_mouse_button_down MouseButton.Left
    && (not !block_selected2)
    && within block
         (float_of_int (get_mouse_x ()))
         (float_of_int (get_mouse_y ()))
  then
    let _ = block_selected2 := true in
    get_mouse_y () - 20
  else
    let _ = block_selected2 := false in
    int_of_float (Rectangle.y block)

(** Changes the blocks position if the mouse is clicking on it*)
let change_block_position block =
  let x' = update_x block in
  let y' = update_y block in
  change_rect block (float_of_int x') (float_of_int y')

(** The block representing the "move" function*)
let move_code_block () =
  let block = move_rect in
  let _ = change_block_position block in
  let _ = draw_rectangle_rec block Color.gold in

  draw_text "Move Cat"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let turn_code_block () =
  let block = turn_rect in
  let _ = change_block_position block in
  let _ = draw_rectangle_rec block Color.green in

  draw_text "Turn Cat"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let move_block color text text_color block =
  let _ = change_block_position block in
  let _ = draw_rectangle_rec block color in

  draw_text text
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 text_color

let testing_station2 () =
  let block = stay_rect2 in
  let _ = draw_rectangle_rec block Color.gold in
  draw_text "Move Cat"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station () =
  let block = stay_rect in
  let _ = draw_rectangle_rec stay_rect Color.green in
  draw_text "Turn Cat"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let start_button () =
  let block = start_button in
  let _ = change_block_position block in
  let _ = draw_rectangle_rounded start_button 0.5 3 Color.skyblue in
  draw_text "Start"
    (int_of_float (Rectangle.x block +. 25.))
    (int_of_float (Rectangle.y block +. 10.))
    16 Color.black

let end_button () =
  let block = end_button in
  let _ = change_block_position block in
  let _ = draw_rectangle_rounded end_button 0.5 3 Color.skyblue in
  draw_text "Run"
    (int_of_float (Rectangle.x block +. 25.))
    (int_of_float (Rectangle.y block +. 10.))
    16 Color.black

let create_move_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected then
    on_screen := Raylib.Rectangle.create 10. 100. 100. 40. :: !on_screen

let draw_on_screen () =
  let _ = List.map (move_block Color.gold "Move Cat" Color.black) !on_screen in
  ()

let rec loop () =
  if window_should_close () then Raylib.close_window
  else
    let _ = 10 in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "OScratch" 10 10 48 Color.blue;
    draw_rectangle 0 60 (Raylib.get_screen_width ()) 3 Color.black;
    draw_rectangle
      (Raylib.get_screen_width () / 4)
      60 3
      (Raylib.get_screen_height ())
      Color.black;
    draw_text "Code Blocks" 10 68 16 Color.black;
    draw_text "Workspace"
      ((Raylib.get_screen_width () / 4) + 10)
      68 16 Color.black;
    if
      within stay_rect2
        (float_of_int (get_mouse_x ()))
        (float_of_int (get_mouse_y ()))
    then create_move_block ();
    draw_on_screen ();
    start_button ();
    testing_station2 ();
    move_code_block ();
    testing_station ();
    turn_code_block ();
    end_button ();
    end_drawing ();
    loop ()
