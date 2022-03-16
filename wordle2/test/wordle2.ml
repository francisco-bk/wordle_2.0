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

let cmp_list lst1 lst2 = 
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let test (name : string) f input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (f input) ~cmp ~printer

let processor_tests = [
    (* Testing green_list *)
    test "Testing green list" 
      (green_list ["l"; "l"; "a"; "m"; "a"] ["l"; "a"; "m"; "a"; "l"]) 
      0 [0] (pp_list Int.to_string) ( cmp_list );
    
    (* Testing yellow_list *)
    test "Whole word matches" 
      (yellow_list ["b"; "a"; "b"; "b"; "b"] ["b"; "b"; "b"; "b"; "b"] [0; 2; 3; 4]) 
      0 [] (pp_list Int.to_string) ( cmp_list );
    test "Whole word matches" 
      (yellow_list ["l"; "l"; "a"; "m"; "a"] ["l"; "a"; "m"; "a"; "l"] [0]) 
      0 [1; 2; 3; 4] (pp_list Int.to_string) ( cmp_list );
    
    (* Testing color_list *)
    test "Same letters only 1 green match" 
      (color_list "llama") "lamal"
      [2;1;1;1;1] (pp_list Int.to_string) ( = );
    test "3 green matches and rest not" 
      (color_list "eevie") "etvoe"
      [2;0;2;0;2] (pp_list Int.to_string) ( = );
    test "Nothing in common" 
      (color_list "salet") "wordy"
      [0;0;0;0;0] (pp_list Int.to_string) ( = );
    test "Similar letters, 3 duplicate in guess" 
      (color_list "baaab") "abbba"
      [1;1;1;0;1] (pp_list Int.to_string) ( = );
    test "Guess has more b's then answer" 
      (color_list "babbb") "bbbbb"
      [2;0;2;2;2] (pp_list Int.to_string) ( = );
    test "Duplicate characters in guess" 
      (color_list "wordy") "rdrpp"
      [0;1;2;0;0] (pp_list Int.to_string) ( = );
    test "10 letter words with multiple yellow matches" 
      (color_list "helloworld") "ajdhouslof"
      [0; 0; 1; 1; 2; 0; 0; 1; 1; 0] (pp_list Int.to_string) ( = );
]

let suite =
  "test suite for Wordle 2.0"
  >::: List.flatten [ processor_tests ]

let _ = run_test_tt_main suite