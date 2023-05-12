open OUnit2
open Game

let test_run_blocks (name: string) (blocks : Project.code_block list) 
(expected_output : string ): test =
  name >:: fun _ -> assert_equal expected_output 
  (Project.run_code_blocks blocks|> Project.grab_string_screen |> Project.list_to_string)~printer: String.escaped

let test_text_grab (name: string)(block: Project.code_block) (expected_ouput : string) : test = 
  name >:: fun _ -> assert_equal (Project.text_grab block) expected_ouput

let test_list_string (name: string)(lst : string list)(expected_output : string) : test = 
  name >:: fun _ -> assert_equal (Project.list_to_string lst)(expected_output) ~printer: String.escaped

let test_turn_block (name: string)(expected_output: string) : test = 
  name >:: fun _ -> assert_equal ( Project.create_turn_test () |> Project.text_grab) expected_output

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

let tests = [
  test_text_grab "test2" (Project.create_turn_test()) "Turn"; 
  test_list_string "testing list to string func" ["hello"] "hello ";
  test_list_string "testing for more than 1 thing" ["hello"; "people"] "hello people "; 
  test_list_string "testing empty list" [] ""; 
  test_turn_block "testing test blocks" "Turn";
  test_run_blocks "test 2 turn blocks" [Project.create_turn_test (); Project.create_turn_test ()] "Turn Turn ";
  test_run_blocks "test 1 turn" [Project.create_turn_test()] "Turn "; 
  test_run_blocks "test 1 turn 1 move" [Project.create_move_test (); Project.create_turn_test()] "Turn Move "; 
]

let suite = "suite" >::: (tests @ (List.rev cat_float_tests))
let _ = run_test_tt_main suite
