open OUnit2
open Aoc_2024

let tests =
  "test suite for aoc 2024"
  >::: [ ("day 1 part a example"
          >:: fun _ -> assert_equal 11 (Day_1.part_a "/home/jared/Projects/aoc_2024/data/examples/day_1a.txt"))
       (*    >:: fun _ -> assert_raises (Failure "nth") (fun () -> nth [ "a" ] 2)) *)
       (* ; ("length of a list" >:: fun _ -> assert_equal 3 (length [ "a"; "b"; "c" ])) *)
       ]
;;

let _ = run_test_tt_main tests
