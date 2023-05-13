open Raylib

type operation =
  | Move
  | Turn
  | Wait

type code_block = {
  op : operation;
  color : Color.t;
  rect : Rectangle.t;
  mutable visible : bool;
  id : int;
  test : bool;
}

let block_id = ref 0
let block_id_test = ref 0
let stay_rect_move = Rectangle.create 10. 100. 100. 40.
let stay_rect_turn = Rectangle.create 10. 150. 100. 40.
let stay_rect_wait = Rectangle.create 10. 200. 100. 40.
let start_button = Rectangle.create 250. 100. 100. 40.
let on_screen = ref []
let string_on_screen = ref []
let block_selected_x = ref false
let block_selected_y = ref false

let setup () =
  init_window 800 450 "[core] example - basic window";
  set_target_fps 60

let draw_cat () = Cat.draw_cat ()

let change_rect rect x y =
  Rectangle.set_x rect x;
  Rectangle.set_y rect y

let within rect x1 y1 =
  let open Rectangle in
  if
    x rect <= x1
    && x1 <= x rect +. width rect
    && y rect <= y1
    && y1 <= y rect +. height rect
  then true
  else false

let update_x block =
  if
    is_mouse_button_down MouseButton.Left
    && (not !block_selected_x)
    && within block
         (float_of_int (get_mouse_x ()))
         (float_of_int (get_mouse_y ()))
  then
    let _ = block_selected_x := true in
    get_mouse_x () - 50
  else
    let _ = block_selected_x := false in
    int_of_float (Rectangle.x block)

let update_y block =
  if
    is_mouse_button_down MouseButton.Left
    && (not !block_selected_y)
    && within block
         (float_of_int (get_mouse_x ()))
         (float_of_int (get_mouse_y ()))
  then
    let _ = block_selected_y := true in
    get_mouse_y () - 20
  else
    let _ = block_selected_y := false in
    int_of_float (Rectangle.y block)

let change_rect_position rect =
  let x' = update_x rect in
  let y' = update_y rect in
  change_rect rect (float_of_int x') (float_of_int y')

let testing_station3 () =
  let block = stay_rect_wait in
  let _ = draw_rectangle_rec block Color.purple in
  draw_text "Wait"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station2 () =
  let block = stay_rect_move in
  let _ = draw_rectangle_rec block Color.gold in
  draw_text "Move Cat"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station () =
  let block = stay_rect_turn in
  let _ = draw_rectangle_rec stay_rect_turn Color.green in
  draw_text "Turn Cat"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let start_button2 () =
  let block = start_button in
  let _ = draw_rectangle_rounded start_button 0.5 3 Color.skyblue in
  draw_text "Start"
    (int_of_float (Rectangle.x block +. 25.))
    (int_of_float (Rectangle.y block +. 10.))
    16 Color.black

let create_move_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Move;
        color = Color.gold;
        rect = Rectangle.create 10. 100. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_turn_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Turn;
        color = Color.green;
        rect = Rectangle.create 10. 150. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_wait_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Wait;
        color = Color.purple;
        rect = Rectangle.create 10. 200. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_code_blocks () =
  if
    within stay_rect_move
      (float_of_int (get_mouse_x ()))
      (float_of_int (get_mouse_y ()))
  then create_move_block ();
  if
    within stay_rect_turn
      (float_of_int (get_mouse_x ()))
      (float_of_int (get_mouse_y ()))
  then create_turn_block ();
  if
    within stay_rect_wait
      (float_of_int (get_mouse_x ()))
      (float_of_int (get_mouse_y ()))
  then create_wait_block ()

let visible_false_helper block =
  let block_rect = block.rect in
  if
    Rectangle.x block_rect > float_of_int (get_screen_width () - 100)
    && Rectangle.y block_rect < float_of_int (get_screen_height () - 40)
  then block.visible <- false

let visible_false () =
  let _ = List.map visible_false_helper !on_screen in
  ()

let remove_block_tc () =
  on_screen := List.filter (fun block -> block.visible = true) !on_screen;
  block_selected_x := false;
  block_selected_y := false

let move_block block =
  let { op; color; rect; _ } = block in
  let _ = change_rect_position rect in
  let _ = draw_rectangle_rec rect color in
  let text =
    match op with
    | Move -> "Move Cat"
    | Turn -> "Turn Cat"
    | Wait -> "Wait"
  in
  draw_text text
    (int_of_float (Rectangle.x rect +. 10.))
    (int_of_float (Rectangle.y rect +. 5.))
    16 Color.black

let draw_on_screen () =
  let _ = List.map move_block !on_screen in
  ()

let sort_exec_order on_screen =
  let compare_block b1 b2 =
    let { op = _; color = _; rect = rect1; visible = _; id = _; test = _ } =
      b1
    in
    let { op = _; color = _; rect = rect2; visible = _; id = _; test = _ } =
      b2
    in
    let y1, y2 = (Rectangle.y rect1, Rectangle.y rect2) in
    if y1 > y2 then 1 else if y1 < y2 then -1 else 0
  in
  List.sort compare_block on_screen

let sort_block_position () =
  let sorted = sort_exec_order !on_screen in
  let curr_y = ref 150. in
  let format_block_pos block =
    let { op = _; color = _; visible = _; rect; id = _; test = _ } = block in
    change_rect rect 250. !curr_y;
    curr_y := !curr_y +. 45.
  in
  let _ = List.map format_block_pos sorted in
  ()

let get_op block = block.op

let text_grab block =
  match get_op block with
  | Turn -> "Turn"
  | Move -> "Move"
  | Wait -> "Wait"

let run_opp op =
  match op with
  | Turn ->
      Cat.change_direction ();
      ()
  | Move ->
      Cat.move_right 2.0;
      ()
  | Wait -> wait_time 5.0

let rec run_code_blocks lst =
  match lst with
  | [] -> ()
  | h :: t ->
      if h.test = true then string_on_screen := text_grab h :: !string_on_screen
      else run_opp (get_op h);
      run_code_blocks t

let run_text () =
  draw_text "Press \"r\" to run" (get_screen_width () - 200) 45 16 Color.purple;
  draw_text "Press \"s\" to sort"
    ((get_screen_width () / 4) + 10)
    (get_screen_height () - 15)
    16 Color.blue;
  draw_text "Code Blocks" 10 68 16 Color.black;
  draw_text "Workspace" ((get_screen_width () / 4) + 10) 68 16 Color.black;
  draw_text "OScratch" 10 10 48 Color.blue

let sort_post () = if is_key_pressed S then sort_block_position ()
let run_block () = if is_key_pressed R then run_code_blocks !on_screen

let setup_view () =
  clear_background Color.raywhite;
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
  draw_text "Trash Can"
    (get_screen_width () - 100)
    (get_screen_height () - 40)
    10 Color.white;
  run_text ()

let rec loop () =
  if window_should_close () then close_window
  else
    let _ = 10 in
    begin_drawing ();
    setup_view ();
    draw_on_screen ();
    create_code_blocks ();
    start_button2 ();
    testing_station2 ();
    testing_station ();
    testing_station3 ();
    visible_false ();
    remove_block_tc ();
    sort_post ();
    end_drawing ();
    print_endline (string_of_int (List.length !on_screen));
    draw_cat ();
    run_block ();

    (*Can test new cat features under here*)
    (* Cat.change_direction (); *)
    (*Cat.move_right 0.5; print_float (Cat.get_x ());*)
    loop ()

let grab_string_screen () = !string_on_screen

let rec list_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ " " ^ list_to_string t

let create_turn_test x y =
  let _ = block_id_test := !block_id_test + 1 in
  {
    op = Turn;
    color = Color.green;
    rect = Rectangle.create x y 100. 40.;
    visible = true;
    id = !block_id_test;
    test = true;
  }

let create_move_test x y =
  let _ = block_id_test := !block_id_test + 1 in
  {
    op = Move;
    color = Color.gold;
    rect = Rectangle.create x y 100. 40.;
    visible = true;
    id = !block_id_test;
    test = true;
  }

let create_wait_test x y =
  let _ = block_id_test := !block_id_test + 1 in
  {
    op = Wait;
    color = Color.purple;
    rect = Rectangle.create x y 100. 40.;
    visible = true;
    id = !block_id_test;
    test = true;
  }
