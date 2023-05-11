open OUnit2

let test_run_blocks (name : string) (name2 : string) =
  name >:: fun _ -> assert_equal name name2



let tests = [
  test_run_blocks "Gabby" "Gabby";
]
let suite = "suite" >::: tests
let _ = run_test_tt_main suite
