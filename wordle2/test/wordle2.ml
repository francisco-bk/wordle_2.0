open OUnit2
open Game
open Processor

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
    let pp_list pp_elt lst =
      let pp_elts lst =
        let rec loop n acc = function
          | [] -> acc
          | [ h ] -> acc ^ pp_elt h
          | h1 :: ( _ :: _ as t') ->
              if n = 100 then acc ^ "..." (* stop printing long list *)
              else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
        in
        loop 0 "" lst
      in
      "[" ^ pp_elts lst ^ "]"

let test (name : string) f input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (f input) ~cmp ~printer

let processor_tests = [
    (* Testing green_list *)
    test "Whole word matches" (green_list ["h"; "e"; "l"; "l"; "o"] ["h"; "e"; "l"; "l"; "o"]) 
    0 [0;1;2;3;4] (pp_list Int.to_string) ( = )
]

let suite =
  "test suite for Wordle 2.0"
  >::: List.flatten [ processor_tests ]

let _ = run_test_tt_main suite