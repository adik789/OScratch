open OUnit2
open Game

let test_run_blocks (name : string) (name2 : string) =
  name >:: fun _ -> assert_equal name name2

let test_cat_floats (name: string) (action : unit) (f : unit -> float) (exp_value : float) = 
  (* Cat.move_right 10.; *)
  action;
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
  test_run_blocks "Gabby" "Gabby";
]

let suite = "suite" >::: (tests @ (List.rev cat_float_tests))
let _ = run_test_tt_main suite
