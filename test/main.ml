open OUnit2
open Raylib
open Game

let test_change_rect (name: string) (rect: Rectangle.t) (x: float)
(y: float): test =
name >:: fun _ -> let _ = Project.change_rect rect x y in
let _ = assert_equal (Rectangle.x rect) x in assert_equal (Rectangle.y rect) y
~printer: string_of_float

let test_within (name: string) (rect: Rectangle.t) (x: float) (y: float)
(expected_ouput: bool): test =
name >:: fun _ -> assert_equal (expected_ouput) (Project.within rect x y)
~printer: string_of_bool

let test_run_blocks (name: string) (blocks : Project.code_block list) 
(expected_output : string ): test =
  name >:: fun _ -> assert_equal expected_output 
  (Project.run_code_blocks blocks|> Project.grab_string_screen
  |> Project.list_to_string)~printer: String.escaped

let test_text_grab (name: string)(block: Project.code_block)
(expected_ouput : string) : test = 
  name >:: fun _ -> assert_equal (Project.text_grab block) expected_ouput

let test_list_string (name: string)(lst: string list)
(expected_output: string) : test = 
  name >:: fun _ -> assert_equal (Project.list_to_string lst)
  (expected_output) ~printer: String.escaped

let test_turn_block (name: string)(expected_output: string) : test = 
  name >:: fun _ -> assert_equal ( Project.create_turn_test 0. 0. |> Project.text_grab) expected_output

let test_sort_exec (name: string) (lst: Project.code_block list)
(expected_output: string) : test = name >:: fun _ -> 
  assert_equal (Project.(lst |> sort_exec_order |> List.map text_grab
  |> list_to_string)) (expected_output) ~printer: String.escaped

let test_cat_floats (name: string) (action : unit) (f : unit -> float) (exp_value : float) = 
  (* Cat.move_right 10.; *)
  action;
  print_float (f ());
  let hello = (f ()) in 
  name >::
  fun _ -> assert_equal (hello) exp_value



let cat_float_tests = [
  (* The tests are executed in reverse order for some reason*)
  test_cat_floats "Move left 10 pixels" (Cat.move_left 10.) (Cat.get_x) 500.;
  test_cat_floats "Move right 10 pixels" (Cat.move_right 10.) (Cat.get_x) 510.;
  test_cat_floats "Move up 10 pixels" (Cat.move_up 10.) (Cat.get_y) 100.;
  test_cat_floats "Move down 10 pixels" (Cat.move_down 10.) (Cat.get_y) 110.;
  ]
let test_rect = Rectangle.create 0. 0. 100. 100.

let change_rect_tests = [
  test_change_rect "To origin" test_rect 0. 0.;
  test_change_rect "Only Change X" test_rect 100. 0.;
  test_change_rect "Only Change Y" test_rect 100. 100.;
  test_change_rect "Half Pixel Change X" test_rect 100.5 100.;
  test_change_rect "Half Pixel Change Y" test_rect 100.5 100.5;
  test_change_rect "Postive X Positive Y" test_rect 200. 200.;
  test_change_rect "Negative X Positive Y" test_rect (-200.) 200.;
  test_change_rect "Positive X Negative Y" test_rect 200. (-200.);
  test_change_rect "Negative X Negative Y" test_rect (-200.) (-200.);
]

let test_rect = Rectangle.create 0. 0. 100. 100.

let within_tests = [
  test_within "Top Left Corner" test_rect 0. 0. true;
  test_within "Top Right Corner" test_rect 100. 0. true;
  test_within "Bottom Left Corner" test_rect 0. 100. true;
  test_within "Bottom Right Corner" test_rect 100. 100. true;
  test_within "Center" test_rect 50. 50. true;
  test_within "Within Bounds" test_rect 25. 75. true;
  test_within "Off by x Only" test_rect 200. 50. false;
  test_within "Off by y Only" test_rect 50. 200. false;
  test_within "Off by x and y" test_rect 200. 200. false;
  test_within "Negative x" test_rect (-50.) 50. false;
  test_within "Negative y" test_rect 50. (-50.) false;
  test_within "Negative x and y" test_rect (-50.) (-50.) false;
  test_within "Slightly off x" test_rect 100.000001 100. false;
  test_within "Slightly off y" test_rect 100. 100.000001 false;
  test_within "Slightly off x and y" test_rect 100.000001 100.000001 false;
]

let turn_0_0 = Project.create_turn_test 0. 0.
let move_0_1 = Project.create_move_test 0. 1.
let wait_1_1 = Project.create_wait_test 1. 1.
let wait_1_2 = Project.create_wait_test 1. 2.
let sorted_10 = [
  Project.create_wait_test 1. 2.; Project.create_turn_test 1. 3.;
  Project.create_move_test 1. 4.; Project.create_wait_test 1. 5.;
  Project.create_wait_test 1. 6.; Project.create_turn_test 1. 7.;
  Project.create_move_test 1. 8.; Project.create_wait_test 1. 9.;
  Project.create_move_test 1. 10.; Project.create_wait_test 1. 11.;
  ]
let unsorted_10 = [
  Project.create_move_test 1. 4.; Project.create_wait_test 1. 5.;
  Project.create_wait_test 1. 2.; Project.create_turn_test 1. 3.;
  Project.create_move_test 1. 8.; Project.create_wait_test 1. 9.;
  Project.create_wait_test 1. 6.; Project.create_turn_test 1. 7.;
  Project.create_move_test 1. 10.; Project.create_wait_test 1. 11.;
  ]

let sorted_10_string = "Wait Turn Move Wait Wait Turn Move Wait Move Wait "

let sorted_100 = [
  Project.create_wait_test 1. 2.; Project.create_turn_test 1. 3.;
  Project.create_move_test 1. 4.; Project.create_wait_test 1. 5.;
  Project.create_wait_test 1. 6.; Project.create_turn_test 1. 7.;
  Project.create_move_test 1. 8.; Project.create_wait_test 1. 9.;
  Project.create_move_test 1. 10.; Project.create_wait_test 1. 11.;
  Project.create_wait_test 1. 12.; Project.create_turn_test 1. 13.;
  Project.create_move_test 1. 14.; Project.create_wait_test 1. 15.;
  Project.create_wait_test 1. 16.; Project.create_turn_test 1. 17.;
  Project.create_move_test 1. 18.; Project.create_wait_test 1. 19.;
  Project.create_move_test 1. 20.; Project.create_wait_test 1. 21.;
  Project.create_wait_test 1. 22.; Project.create_turn_test 1. 23.;
  Project.create_move_test 1. 24.; Project.create_wait_test 1. 25.;
  Project.create_wait_test 1. 26.; Project.create_turn_test 1. 27.;
  Project.create_move_test 1. 28.; Project.create_wait_test 1. 29.;
  Project.create_move_test 1. 30.; Project.create_wait_test 1. 31.;
  Project.create_wait_test 1. 32.; Project.create_turn_test 1. 33.;
  Project.create_move_test 1. 34.; Project.create_wait_test 1. 35.;
  Project.create_wait_test 1. 36.; Project.create_turn_test 1. 37.;
  Project.create_move_test 1. 38.; Project.create_wait_test 1. 39.;
  Project.create_move_test 1. 40.; Project.create_wait_test 1. 41.;
  Project.create_wait_test 1. 42.; Project.create_turn_test 1. 43.;
  Project.create_move_test 1. 44.; Project.create_wait_test 1. 45.;
  Project.create_wait_test 1. 46.; Project.create_turn_test 1. 47.;
  Project.create_move_test 1. 48.; Project.create_wait_test 1. 49.;
  Project.create_move_test 1. 50.; Project.create_wait_test 1. 51.;
  Project.create_wait_test 1. 52.; Project.create_turn_test 1. 53.;
  Project.create_move_test 1. 54.; Project.create_wait_test 1. 55.;
  Project.create_wait_test 1. 56.; Project.create_turn_test 1. 57.;
  Project.create_move_test 1. 58.; Project.create_wait_test 1. 59.;
  Project.create_move_test 1. 60.; Project.create_wait_test 1. 61.;
  Project.create_wait_test 1. 62.; Project.create_turn_test 1. 63.;
  Project.create_move_test 1. 64.; Project.create_wait_test 1. 65.;
  Project.create_wait_test 1. 66.; Project.create_turn_test 1. 67.;
  Project.create_move_test 1. 68.; Project.create_wait_test 1. 69.;
  Project.create_move_test 1. 70.; Project.create_wait_test 1. 71.;
  Project.create_wait_test 1. 72.; Project.create_turn_test 1. 73.;
  Project.create_move_test 1. 74.; Project.create_wait_test 1. 75.;
  Project.create_wait_test 1. 76.; Project.create_turn_test 1. 77.;
  Project.create_move_test 1. 78.; Project.create_wait_test 1. 79.;
  Project.create_move_test 1. 80.; Project.create_wait_test 1. 81.;
  Project.create_wait_test 1. 82.; Project.create_turn_test 1. 83.;
  Project.create_move_test 1. 84.; Project.create_wait_test 1. 85.;
  Project.create_wait_test 1. 86.; Project.create_turn_test 1. 87.;
  Project.create_move_test 1. 88.; Project.create_wait_test 1. 89.;
  Project.create_move_test 1. 90.; Project.create_wait_test 1. 91.;
  Project.create_wait_test 1. 92.; Project.create_turn_test 1. 93.;
  Project.create_move_test 1. 94.; Project.create_wait_test 1. 95.;
  Project.create_wait_test 1. 96.; Project.create_turn_test 1. 97.;
  Project.create_move_test 1. 98.; Project.create_wait_test 1. 99.;
  ]

  let unsorted_100 = [
  Project.create_wait_test 1. 42.; Project.create_turn_test 1. 43.;
  Project.create_move_test 1. 44.; Project.create_wait_test 1. 45.;
  Project.create_wait_test 1. 46.; Project.create_turn_test 1. 47.;
  Project.create_move_test 1. 48.; Project.create_wait_test 1. 49.;
  Project.create_move_test 1. 50.; Project.create_wait_test 1. 51.;
  Project.create_wait_test 1. 2.; Project.create_turn_test 1. 3.;
  Project.create_move_test 1. 4.; Project.create_wait_test 1. 5.;
  Project.create_wait_test 1. 6.; Project.create_turn_test 1. 7.;
  Project.create_move_test 1. 8.; Project.create_wait_test 1. 9.;
  Project.create_move_test 1. 10.; Project.create_wait_test 1. 11.;
  Project.create_wait_test 1. 12.; Project.create_turn_test 1. 13.;
  Project.create_move_test 1. 14.; Project.create_wait_test 1. 15.;
  Project.create_wait_test 1. 16.; Project.create_turn_test 1. 17.;
  Project.create_move_test 1. 18.; Project.create_wait_test 1. 19.;
  Project.create_move_test 1. 20.; Project.create_wait_test 1. 21.;
  Project.create_wait_test 1. 22.; Project.create_turn_test 1. 23.;
  Project.create_move_test 1. 24.; Project.create_wait_test 1. 25.;
  Project.create_wait_test 1. 26.; Project.create_turn_test 1. 27.;
  Project.create_move_test 1. 28.; Project.create_wait_test 1. 29.;
  Project.create_wait_test 1. 92.; Project.create_turn_test 1. 93.;
  Project.create_move_test 1. 94.; Project.create_wait_test 1. 95.;
  Project.create_wait_test 1. 96.; Project.create_turn_test 1. 97.;
  Project.create_move_test 1. 98.; Project.create_wait_test 1. 99.;
  Project.create_move_test 1. 30.; Project.create_wait_test 1. 31.;
  Project.create_wait_test 1. 32.; Project.create_turn_test 1. 33.;
  Project.create_move_test 1. 34.; Project.create_wait_test 1. 35.;
  Project.create_wait_test 1. 36.; Project.create_turn_test 1. 37.;
  Project.create_move_test 1. 38.; Project.create_wait_test 1. 39.;
  Project.create_move_test 1. 40.; Project.create_wait_test 1. 41.;
  Project.create_wait_test 1. 52.; Project.create_turn_test 1. 53.;
  Project.create_move_test 1. 54.; Project.create_wait_test 1. 55.;
  Project.create_wait_test 1. 56.; Project.create_turn_test 1. 57.;
  Project.create_move_test 1. 58.; Project.create_wait_test 1. 59.;
  Project.create_move_test 1. 60.; Project.create_wait_test 1. 61.;
  Project.create_wait_test 1. 62.; Project.create_turn_test 1. 63.;
  Project.create_move_test 1. 64.; Project.create_wait_test 1. 65.;
  Project.create_wait_test 1. 66.; Project.create_turn_test 1. 67.;
  Project.create_move_test 1. 68.; Project.create_wait_test 1. 69.;
  Project.create_move_test 1. 70.; Project.create_wait_test 1. 71.;
  Project.create_wait_test 1. 82.; Project.create_turn_test 1. 83.;
  Project.create_move_test 1. 84.; Project.create_wait_test 1. 85.;
  Project.create_wait_test 1. 86.; Project.create_turn_test 1. 87.;
  Project.create_move_test 1. 88.; Project.create_wait_test 1. 89.;
  Project.create_move_test 1. 90.; Project.create_wait_test 1. 91.;
  Project.create_wait_test 1. 72.; Project.create_turn_test 1. 73.;
  Project.create_move_test 1. 74.; Project.create_wait_test 1. 75.;
  Project.create_wait_test 1. 76.; Project.create_turn_test 1. 77.;
  Project.create_move_test 1. 78.; Project.create_wait_test 1. 79.;
  Project.create_move_test 1. 80.; Project.create_wait_test 1. 81.;
  ]


let sorted_100_string = "Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait Move Wait Wait Turn Move Wait Wait Turn Move Wait "

let sort_exec_tests = [
  test_sort_exec "Empty List" [] "";
  test_sort_exec "turn at 0 0" ([turn_0_0]) "Turn ";
  test_sort_exec "move at 0 1" ([move_0_1]) "Move ";
  test_sort_exec "wait at 0 2" ([wait_1_2]) "Wait ";
  test_sort_exec "turn at 0 0, move at 0 1" ([turn_0_0; move_0_1]) "Turn Move ";
  test_sort_exec "move at 0 1, turn at 0 0" ([move_0_1; turn_0_0]) "Turn Move ";
  test_sort_exec "turn at 0 0, wait at 1 2" ([turn_0_0; wait_1_2]) "Turn Wait ";
  test_sort_exec "wait at 1 2, move at 0 1" ([wait_1_2; move_0_1]) "Move Wait ";
  test_sort_exec "Correct y order" ([turn_0_0; move_0_1; wait_1_2]) "Turn Move Wait ";
  test_sort_exec "Scrambled y order" ([wait_1_2; turn_0_0; move_0_1; ]) "Turn Move Wait ";
  test_sort_exec "Reverse y order" ([wait_1_2; move_0_1; turn_0_0]) "Turn Move Wait ";
  test_sort_exec "Same y Coords" ([wait_1_1; move_0_1]) "Wait Move ";
  test_sort_exec "Same y Coords Different Order" ([move_0_1; wait_1_1]) "Move Wait ";
  test_sort_exec "Same Block" ([move_0_1; move_0_1]) "Move Move ";
  test_sort_exec "10 Blocks Sorted" sorted_10 sorted_10_string;
  test_sort_exec "10 Blocks Unsorted" unsorted_10 sorted_10_string;
  test_sort_exec "A Lot of Blocks Unsorted" sorted_100 sorted_100_string;
  test_sort_exec "A Lot of Blocks Unsorted" unsorted_100 sorted_100_string;
]
let tests = [
  test_text_grab "test2" (turn_0_0) "Turn"; 
  test_list_string "testing list to string func" ["hello"] "hello ";
  test_list_string "testing for more than 1 thing" ["hello"; "people"] "hello people "; 
  test_list_string "testing empty list" [] ""; 
  test_turn_block "testing test blocks" "Turn";
  test_run_blocks "test 2 turn blocks" [turn_0_0; turn_0_0] "Turn Turn ";
  test_run_blocks "test 1 turn" [turn_0_0] "Turn "; 
  test_run_blocks "test 1 turn 1 move" [move_0_1; turn_0_0] "Turn Move Turn Turn "; 
]

let suite = "suite" >::: List.flatten [
  change_rect_tests;
  within_tests; sort_exec_tests;
  tests; cat_float_tests
  ]
  
let _ = run_test_tt_main suite
