open OUnit2
open Game
open Processor

let test (name : string) f input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (f input) ~cmp ~printer

let processor_tests = [
    
]

let suite =
  "test suite for Wordle 2.0"
  >::: List.flatten [ processor_tests ]

let _ = run_test_tt_main suite