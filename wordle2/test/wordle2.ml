open OUnit2
open Game
open Processor
open Load
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

(** [test name f input ex_output printer cmp] makes a test named [name]
testing function [f] with input [input] (NOTE: to use multiple inputs,
put the other inputs with the function like so (f a) b), [ex_output] represents
the expected output, printer takes the return type of the function to string,
and [cmp] is the comparator to compare the results*)
let test (name : string) f input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (f input) ~cmp ~printer

let processor_tests = [
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
(*  ***************************************************************************)

(* tests for the Load module *)
let i a = a
let parse_dict_tests = [
  test "parse empty is empty" (parse_dict) [] [] (pp_list i) ( = );
test "parse ['a'] is ['a']" (parse_dict) ["a"] ["a"] (pp_list i) ( = );
test "parse [' a '] is ['a']" (parse_dict) [" a "] ["a"] (pp_list i) ( = );
test "parse [' a'] is ['a']" (parse_dict) [" a"] ["a"] (pp_list i) ( = );
test "parse ['a';'b'] is ['a';'b']" (parse_dict) ["a";"b"] ["a";"b"] (pp_list i) ( = );
test "parse ['a';' b '] is ['a';'b']" (parse_dict) ["a";" b "] ["a";"b"] (pp_list i) ( = );]


let  choose_word_length_tests = [
  test "choose empty is empty" (choose_word_length 5) [] [] (pp_list i) ( = );
  test "choose 1 ['a'] is ['a']" (choose_word_length 1) ["a"] ["a"] (pp_list i) ( = );
  test "choose 1 ['ab'] is []" (choose_word_length 1) ["ab"] [] (pp_list i) ( = );
  test "choose 2 ['ab'] is ['ab']" (choose_word_length 2) ["ab"] ["ab"] (pp_list i) ( = );
  test "choose 1 ['a';'ab'] is ['a']" (choose_word_length 1) ["a";"ab"] ["a"] (pp_list i) ( = );
  test "choose 2 ['a';'ab'] is ['ab']" (choose_word_length 2) ["a";"ab"] ["ab"] (pp_list i) ( = );
  test "choose 3 ['a';'ab'] is []" (choose_word_length 3) ["a";"ab"] [] (pp_list i) ( = );
]
let load_tests = List.flatten [parse_dict_tests;choose_word_length_tests]
 

(*  ***************************************************************************)
let suite =
  "test suite for Wordle 2.0"
  >::: List.flatten [ processor_tests ; load_tests]

let _ = run_test_tt_main suite