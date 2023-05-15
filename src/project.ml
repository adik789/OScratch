open Raylib

type operation =
  | Right
  | Left
  | Up
  | Down
  | Turn
  | Wait
  | Color
  | Grow
  | Shrink

type code_block = {
  op : operation;
  color : Color.t;
  rect : Rectangle.t;
  mutable visible : bool;
  id : int;
  test : bool;
}

let music_muted = ref false
let default_move = 10.0
let block_id = ref 0
let block_id_test = ref 0
let stay_rect_right = Rectangle.create 10. 100. 100. 40.
let stay_rect_left = Rectangle.create 10. 150. 100. 40.
let stay_rect_up = Rectangle.create 10. 200. 100. 40.
let stay_rect_down = Rectangle.create 10. 250. 100. 40.
let stay_rect_turn = Rectangle.create 10. 300. 100. 40.
let stay_rect_wait = Rectangle.create 10. 350. 100. 40.
let stay_rect_color = Rectangle.create 10. 400. 100. 40.
let stay_rect_grow = Rectangle.create 10. 450. 100. 40.
let stay_rect_shrink = Rectangle.create 10. 500. 100. 40.
let start_button = Rectangle.create 275. 100. 100. 40.
let reset_button = Rectangle.create 900. 70. 100. 30.
let info_rect = Rectangle.create 500. 500. 600. 600.

let stationary_blocks =
  [
    stay_rect_right;
    stay_rect_left;
    stay_rect_up;
    stay_rect_down;
    stay_rect_turn;
    stay_rect_wait;
    stay_rect_color;
  ]

let on_screen = ref []
let string_on_screen = ref []
let ref_test = ref 0
let block_selected_x = ref false
let block_selected_y = ref false
let info_status = ref false

let setup () =
  init_window 1000 800 "[core] example - basic window";
  set_target_fps 200;
  init_audio_device ();
  let music = load_music_stream "resources/oscratch.mp3" in
  set_music_volume music 0.5;
  play_music_stream music;
  music

let draw_cat () = Cat.draw_cat ()
let reset_cat () = Cat.reset_cat ()

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

let testing_station_wait () =
  let block = stay_rect_wait in
  let _ = draw_rectangle_rec block Color.purple in
  draw_text "Wait"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let info_extra () =
  let block = info_rect in
  draw_text "Click h to run 1 block at a time"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 140.))
    20 Color.black;
  draw_text "Click c to clear code blocks"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 180.))
    20 Color.black;
  draw_text "Click i to exit this window"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 220.))
    20 Color.black

let testing_info_wait () =
  let block = info_rect in
  let _ = draw_rectangle_rec block Color.pink in
  draw_text "Instructions"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 20.))
    25 Color.red;
  draw_text "Drag Code Blocks Onto Workspace"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 60.))
    20 Color.black;
  draw_text "Click s to arrange code blocks"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 100.))
    20 Color.black;
  info_extra ()

let make_info_pop () = if is_key_pressed I then info_status := not !info_status
let make_pop () = if !info_status == true then testing_info_wait ()

let testing_station_right () =
  let block = stay_rect_right in
  let _ = draw_rectangle_rec block Color.gold in
  draw_text "Move Right"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station_left () =
  let block = stay_rect_left in
  let _ = draw_rectangle_rec block Color.orange in
  draw_text "Move Left"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station_up () =
  let block = stay_rect_up in
  let _ = draw_rectangle_rec block Color.red in
  draw_text "Move Up"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station_down () =
  let block = stay_rect_down in
  let _ = draw_rectangle_rec block Color.blue in
  draw_text "Move Down"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station_turn () =
  let block = stay_rect_turn in
  let _ = draw_rectangle_rec stay_rect_turn Color.green in
  draw_text "Turn Cat"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station_color () =
  let block = stay_rect_color in
  let _ = draw_rectangle_rec stay_rect_color Color.gray in
  draw_text "Color"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let testing_station_grow () =
  let block = stay_rect_grow in
  let _ = draw_rectangle_rec stay_rect_grow Color.darkpurple in
  draw_text "Grow"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.white

let testing_station_shrink () =
  let block = stay_rect_shrink in
  let _ = draw_rectangle_rec stay_rect_shrink Color.pink in
  draw_text "Shrink"
    (int_of_float (Rectangle.x block +. 10.))
    (int_of_float (Rectangle.y block +. 5.))
    16 Color.black

let create_move_right_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Right;
        color = Color.gold;
        rect = Rectangle.create 10. 100. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_move_left_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Left;
        color = Color.orange;
        rect = Rectangle.create 10. 150. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_move_up_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Up;
        color = Color.red;
        rect = Rectangle.create 10. 200. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_move_down_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Down;
        color = Color.blue;
        rect = Rectangle.create 10. 250. 100. 40.;
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
        rect = Rectangle.create 10. 300. 100. 40.;
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
        rect = Rectangle.create 10. 350. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_color_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Color;
        color = Color.gray;
        rect = Rectangle.create 10. 400. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_grow_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Grow;
        color = Color.darkpurple;
        rect = Rectangle.create 10. 450. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_shrink_block () =
  if is_mouse_button_pressed MouseButton.Left && not !block_selected_x then
    let _ = block_id := !block_id + 1 in
    on_screen :=
      {
        op = Shrink;
        color = Color.pink;
        rect = Rectangle.create 10. 500. 100. 40.;
        visible = true;
        id = !block_id;
        test = false;
      }
      :: !on_screen

let create_code_blocks () =
  let mousex = float_of_int (get_mouse_x ()) in
  let mousey = float_of_int (get_mouse_y ()) in
  if within stay_rect_right mousex mousey then create_move_right_block ();
  if within stay_rect_left mousex mousey then create_move_left_block ();
  if within stay_rect_up mousex mousey then create_move_up_block ();
  if within stay_rect_down mousex mousey then create_move_down_block ();
  if within stay_rect_turn mousex mousey then create_turn_block ();
  if within stay_rect_wait mousex mousey then create_wait_block ();
  if within stay_rect_color mousex mousey then create_color_block ();
  if within stay_rect_grow mousex mousey then create_grow_block ();
  if within stay_rect_shrink mousex mousey then create_shrink_block ()

let visible_false_helper block =
  let block_rect = block.rect in
  if
    Rectangle.x block_rect > float_of_int (get_screen_width () - 120)
    && Rectangle.y block_rect > float_of_int (get_screen_height () - 50)
  then (
    let trash_sound = load_sound "resources/trash.wav" in
    set_sound_volume trash_sound 1.0;
    play_sound trash_sound;
    block.visible <- false)

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
    | Right -> "Move Right"
    | Left -> "Move Left"
    | Up -> "Move Up"
    | Down -> "Move Down"
    | Turn -> "Turn Cat"
    | Wait -> "Wait"
    | Color -> "Color"
    | Grow -> "Grow"
    | Shrink -> "Shrink"
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

let sort_block_position on_screen =
  let sorted = sort_exec_order on_screen in
  let curr_y = ref 150. in
  let format_block_pos block =
    let { op = _; color = _; visible = _; rect; id = _; test = _ } = block in
    change_rect rect 275. !curr_y;
    curr_y := !curr_y +. 45.;
    rect
  in
  List.map format_block_pos sorted

let get_op block = block.op

let text_grab block =
  match get_op block with
  | Turn -> "Turn"
  | Right -> "Move Right"
  | Left -> "Move Left"
  | Up -> "Move Up"
  | Down -> "Move Down"
  | Wait -> "Wait"
  | Color -> "Color"
  | Grow -> "Grow"
  | Shrink -> "Shrink"

let color_list =
  [
    Raylib.Color.red;
    Raylib.Color.green;
    Raylib.Color.white;
    Raylib.Color.purple;
    Raylib.Color.beige;
    Raylib.Color.darkblue;
    Raylib.Color.darkgreen;
    Raylib.Color.pink;
    Raylib.Color.brown;
    Raylib.Color.yellow;
    Raylib.Color.orange;
  ]

let run_opp op =
  match op with
  | Turn -> Cat.change_direction ()
  | Right -> Cat.move_right default_move
  | Left -> Cat.move_left default_move
  | Up -> Cat.move_up default_move
  | Down -> Cat.move_down default_move
  | Wait -> wait_time 2.0
  | Color ->
      let index = Random.int 11 in
      Cat.change_color (List.nth color_list index)
  | Grow -> Cat.shrink 1.005
  | Shrink -> Cat.grow 1.005

let rec run_code_blocks lst =
  match lst with
  | [] -> ()
  | h :: t ->
      if h.test = true then string_on_screen := text_grab h :: !string_on_screen
      else run_opp (get_op h);
      run_code_blocks t

(*let update_test () = ref_test := sort_exec_order ()*)

let run_head () =
  if !on_screen <> [] then (
    let sorted = sort_exec_order !on_screen in
    let block = List.nth sorted !ref_test in
    (if block.test = false then run_opp (get_op (List.nth sorted !ref_test))
    else
      let _ = text_grab block :: !string_on_screen in
      ());
    ref_test := !ref_test + 1)

let update_ref_test () =
  if !ref_test >= List.length !on_screen then ref_test := 0 else ()

let run_text () =
  draw_text "Hold \"R\" to Run" (get_screen_width () - 200) 45 16 Color.purple;
  draw_text "Press \"S\" to Sort"
    ((get_screen_width () / 4) + 10)
    (get_screen_height () - 15)
    16 Color.blue;
  draw_text "Press \"M\" to Mute" 10 (get_screen_height () - 15) 16 Color.blue;
  draw_text "Code Blocks" 10 68 16 Color.black;
  draw_text "Workspace" ((get_screen_width () / 4) + 10) 68 16 Color.black;
  draw_text "OScratch" 10 10 48 Color.blue;
  draw_text "Press \"H\" to Step" (get_screen_width () - 200) 30 16 Color.pink;
  draw_text "Press \"I\" for Instructions" 10
    (get_screen_height () - 50)
    16 Color.red;
  draw_text "Press \"C\" to Clear"
    (get_screen_width () - 200)
    15 16 Color.darkpurple

let sort_post () =
  if is_key_pressed S then (
    let sort_sound = load_sound "resources/sort.wav" in
    set_sound_volume sort_sound 1.0;
    play_sound sort_sound;
    let _ = sort_block_position !on_screen in
    ())

let clear_on_screen () = on_screen := []

let clear_all () =
  if is_key_pressed C then (
    let clear_sound = load_sound "resources/clear.wav" in
    set_sound_volume clear_sound 1.0;
    play_sound clear_sound;
    clear_on_screen ())

let mute_music music =
  if is_key_pressed M then
    if !music_muted then
      let _ = music_muted := false in
      play_music_stream music
    else
      let _ = music_muted := true in
      stop_music_stream music

let get_rect lst =
  List.map
    (fun block ->
      let { op = _; color = _; rect; visible = _; id = _; test = _ } = block in
      rect)
    lst

let check_click () =
  let rec check_within lst =
    let mousex = float_of_int (get_mouse_x ()) in
    let mousey = float_of_int (get_mouse_y ()) in
    match lst with
    | [] -> false
    | h :: t -> within h mousex mousey || check_within t
  in
  check_within (get_rect !on_screen) || check_within stationary_blocks

let click_sound () =
  if is_mouse_button_pressed MouseButton.Left && check_click () then (
    let click_sound = load_sound "resources/click1.wav" in
    set_sound_volume click_sound 1.0;
    play_sound click_sound)
  else if is_mouse_button_released MouseButton.Left && check_click () then (
    let click_sound2 = load_sound "resources/click2.wav" in
    set_sound_volume click_sound2 1.0;
    play_sound click_sound2)

let run_block () = if is_key_down R then run_head ()

let run_head_block () =
  if is_key_pressed H then (
    let step_sound = load_sound "resources/step.wav" in
    set_sound_volume step_sound 1.0;
    play_sound step_sound;
    run_head ())

let reset_button () =
  let block = reset_button in
  let _ = draw_rectangle_rounded reset_button 0.5 3 Color.red in
  draw_text "Reset"
    (int_of_float (Rectangle.x block +. 25.))
    (int_of_float (Rectangle.y block +. 10.))
    16 Color.white;
  let mousex = float_of_int (get_mouse_x ()) in
  let mousey = float_of_int (get_mouse_y ()) in
  if within reset_button mousex mousey && is_mouse_button_down MouseButton.Left
  then reset_cat ()

let start_button () =
  let block = start_button in
  let _ = draw_rectangle_rounded start_button 0.5 3 Color.skyblue in
  draw_text "Start"
    (int_of_float (Rectangle.x block +. 25.))
    (int_of_float (Rectangle.y block +. 10.))
    16 Color.black;
  let mousex = float_of_int (get_mouse_x ()) in
  let mousey = float_of_int (get_mouse_y ()) in
  if
    within start_button mousex mousey
    && is_mouse_button_pressed MouseButton.Left
  then run_code_blocks !on_screen

let setup_view () =
  clear_background Color.raywhite;
  draw_rectangle 0 60 (get_screen_width ()) 3 Color.black;
  draw_rectangle
    (get_screen_width () - 125)
    (get_screen_height () - 55)
    125 55 Color.red;
  draw_rectangle
    (get_screen_width () / 4)
    60 3 (get_screen_height ()) Color.black;
  draw_rectangle (*For the right most cat zone*)
    (get_screen_width () / 2)
    60 3 (get_screen_height ()) Color.black;
  draw_text "Trash Can"
    (get_screen_width () - 100)
    (get_screen_height () - 40)
    16 Color.white;
  run_text ();
  sort_post ();
  clear_all ();
  click_sound ()

let setup_stationary_blocks () =
  reset_button ();
  start_button ();
  testing_station_right ();
  testing_station_left ();
  testing_station_up ();
  testing_station_down ();
  testing_station_turn ();
  testing_station_wait ();
  testing_station_color ();
  testing_station_grow ();
  testing_station_shrink ()

let rec loop music () =
  match Raylib.window_should_close () with
  | true ->
      let open Raylib in
      unload_music_stream music;
      close_audio_device ();
      close_window ()
  | false ->
      update_music_stream music;
      mute_music music;
      begin_drawing ();
      setup_view ();
      draw_on_screen ();
      setup_stationary_blocks ();
      create_code_blocks ();
      visible_false ();
      remove_block_tc ();
      make_info_pop ();
      make_pop ();
      end_drawing ();

      run_block ();
      draw_cat ();
      update_ref_test ();
      run_head_block ();
      loop music ()

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

let match_color opp =
  match opp with
  | Turn -> Color.green
  | Right -> Color.gold
  | Left -> Color.orange
  | Up -> Color.red
  | Down -> Color.blue
  | Wait -> Color.purple
  | Color -> Color.gray
  | Grow -> Color.darkpurple
  | Shrink -> Color.pink

let match_opp_string s =
  match s with
  | "Turn" -> Turn
  | "Move Right" -> Right
  | "Move Left" -> Left
  | "Move Up" -> Up
  | "Move Down" -> Down
  | "Wait" -> Wait
  | "Color" -> Color
  | _ -> failwith "Invalid Function"

let create_test_block opp x y =
  let opp2 = match_opp_string opp in
  let _ = block_id_test := !block_id_test + 1 in
  {
    op = opp2;
    color = match_color opp2;
    rect = Rectangle.create x y 100. 40.;
    visible = true;
    id = !block_id_test;
    test = true;
  }

let create_right_test x y =
  let _ = block_id_test := !block_id_test + 1 in
  {
    op = Right;
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
