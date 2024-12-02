open OUnit2
open Aoc_2024

let tests =
  "test suite for aoc 2024"
  >::: [ ("day 1 part a example"
          >:: fun _ ->
          assert_equal
            11
            (Day_1.part_a "/home/jared/Projects/aoc_2024/data/examples/day_1a.txt"))
       ; ("day 1 part b example"
          >:: fun _ ->
          assert_equal
            31
            (Day_1.part_b "/home/jared/Projects/aoc_2024/data/examples/day_1b.txt"))
       ; ("day 2 part a example"
          >:: fun _ ->
          assert_equal
            2
            (Day_2.part_a "/home/jared/Projects/aoc_2024/data/examples/day_2a.txt"))
       ; ("day 2 part b example"
          >:: fun _ ->
          assert_equal
            4
            (Day_2.part_b "/home/jared/Projects/aoc_2024/data/examples/day_2b.txt"))
       ]
;;

let _ = run_test_tt_main tests
