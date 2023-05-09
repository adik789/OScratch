open Raylib

let setup () =
  init_window 800 450 "[core] example - basic window";
  set_target_fps 60

let change_rect rect x y =
  Rectangle.set_x rect x;
  Rectangle.set_y rect y

let draw_cat () = Cat.draw_cat ()

type operation =
  | Move
  | Turn

type code_block = {
  op : operation;
  color : Color.t;
  rect : Rectangle.t;
  id : int;
}

let block_id = ref 0

let within rect x1 y1 =
  let open Rectangle in
  if
    x rect <= x1
    && x1 <= x rect +. width rect
    && y rect <= y1
    && y1 <= y rect +. height rect
  then true
  else false

let stay_rect2 = Rectangle.create 10. 100. 100. 40.
let stay_rect = Rectangle.create 10. 150. 100. 40.
let start_button = Rectangle.create 10. 200. 100. 40.
let stay_rect3 = Rectangle.create 10. 200. 100. 40.
let end_button = Rectangle.create 10. 250. 100. 40.
let stay_rect4 = Rectangle.create 10. 200. 100. 40.
let on_screen = ref []
let default_x = 10
let default_y = 100
let defaul_spacing = 100
let block_selected = ref false
let block_selected2 = ref false

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
let change_rect_position rect =
  let x' = update_x rect in
  let y' = update_y rect in
  change_rect rect (float_of_int x') (float_of_int y')

let move_block block =
  let { op; color; rect; _ } = block in
  let _ = change_rect_position rect in
  let _ = draw_rectangle_rec rect color in
  let text =
    match op with
    | Move -> "Move Cat"
    | Turn -> "Turn Cat"
  in
  draw_text text
    (int_of_float (Rectangle.x rect +. 10.))
    (int_of_float (Rectangle.y rect +. 5.))
    16 Color.black

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
  let _ = change_rect_position block in
  let _ = draw_rectangle_rounded start_button 0.5 3 Color.skyblue in
  draw_text "Start"
    (int_of_float (Rectangle.x block +. 25.))
    (int_of_float (Rectangle.y block +. 10.))
    16 Color.black

let end_button () =
  let block = end_button in
  let _ = change_rect_position block in
  let _ = draw_rectangle_rounded end_button 0.5 3 Color.skyblue in
  draw_text "Run"
    (int_of_float (Rectangle.x block +. 25.))
    (int_of_float (Rectangle.y block +. 10.))
    16 Color.black

let create_move_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Move;
        color = Color.gold;
        rect = Rectangle.create 10. 100. 100. 40.;
        id = !block_id;
      }
      :: !on_screen

let create_turn_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Turn;
        color = Color.green;
        rect = Rectangle.create 10. 150. 100. 40.;
        id = !block_id;
      }
      :: !on_screen

let draw_on_screen () =
  let _ = List.map move_block !on_screen in
  ()

let sort_exec_order () =
  let compare_block b1 b2 =
    let { op = _; color = _; rect = rect1; id = _ } = b1 in
    let { op = _; color = _; rect = rect2; id = _ } = b2 in
    let y1, y2 = (Rectangle.y rect1, Rectangle.y rect2) in
    if y1 > y2 then 1 else if y1 < y2 then -1 else 0
  in
  List.sort compare_block !on_screen

let sort_block_position () =
  let sorted = sort_exec_order () in
  let curr_y = ref 100. in
  let format_block_pos block =
    let { op = _; color = _; rect; id = _ } = block in
    change_rect rect 250. !curr_y;
    curr_y := !curr_y +. 45.
  in
  let _ = List.map format_block_pos sorted in
  ()

let sort_post () = if is_key_pressed S then sort_block_position ()

let rec loop () =
  if window_should_close () then close_window
  else
    let _ = 10 in
    begin_drawing ();
    clear_background Color.raywhite;
    draw_text "OScratch" 10 10 48 Color.blue;
    draw_rectangle 0 60 (get_screen_width ()) 3 Color.black;
    draw_rectangle
      (get_screen_width () - 105)
      (get_screen_height () - 45)
      105 45 Color.red;
    draw_rectangle
      (get_screen_width () / 4)
      60 3 (get_screen_height ()) Color.black;
    draw_rectangle (*For the right most cat zone*)
      (get_screen_width () / 2)
      60 3 (get_screen_height ()) Color.black;
    draw_text "Code Blocks" 10 68 16 Color.black;
    draw_text "Workspace" ((get_screen_width () / 4) + 10) 68 16 Color.black;
    draw_text "Trash Can"
      (get_screen_width () - 100)
      (get_screen_height () - 40)
      10 Color.white;
    if
      within stay_rect2
        (float_of_int (get_mouse_x ()))
        (float_of_int (get_mouse_y ()))
    then create_move_block ();
    if
      within stay_rect
        (float_of_int (get_mouse_x ()))
        (float_of_int (get_mouse_y ()))
    then create_turn_block ();
    draw_on_screen ();
    start_button ();
    testing_station2 ();
    testing_station ();
    end_button ();
    sort_post ();
    end_drawing ();
    print_endline (string_of_int (List.length !on_screen));
    draw_cat ();
    (*Can test new cat features under here*)
    Cat.change_direction ();
    Cat.move_right 0.5;
    loop ()
