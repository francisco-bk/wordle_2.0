open OUnit2
open Game
open Processor
open Load
open HintEngine

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
and [cmp] is the comparator to compare the results *)
let test (name : string) f input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (f input) ~cmp ~printer

(** [test2 name input ex_output printer cmp] makes a test named [name] to verify
[input] against [ex_output]. This combines f and input from [test]. *)
let test2 (name : string) input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output input ~cmp ~printer

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

(*****************************************************************)
(* tests for the Load module *)
(*****************************************************************)

let i a = a
let parse_dict_tests = [
  test "parse empty is empty" (parse_dict) [] [] (pp_list i) ( = );
test "parse ['a'] is ['a']" (parse_dict) ["a"] ["a"] (pp_list i) ( = );
test "parse [' a '] is ['a']" (parse_dict) [" a "] ["a"] (pp_list i) ( = );
test "parse [' a'] is ['a']" (parse_dict) [" a"] ["a"] (pp_list i) ( = );
test "parse ['a';'b'] is ['a';'b']" (parse_dict) ["a";"b"] ["a";"b"] (pp_list i) ( = );
test "parse ['a';' b '] is ['a';'b']" (parse_dict) ["a";" b "] ["a";"b"] (pp_list i) ( = );]


let choose_word_length_tests = [
  test "choose empty is empty" (choose_word_length 5) [] [] (pp_list i) ( = );
  test "choose 1 ['a'] is ['a']" (choose_word_length 1) ["a"] ["a"] (pp_list i) ( = );
  test "choose 1 ['ab'] is []" (choose_word_length 1) ["ab"] [] (pp_list i) ( = );
  test "choose 2 ['ab'] is ['ab']" (choose_word_length 2) ["ab"] ["ab"] (pp_list i) ( = );
  test "choose 1 ['a';'ab'] is ['a']" (choose_word_length 1) ["a";"ab"] ["a"] (pp_list i) ( = );
  test "choose 2 ['a';'ab'] is ['ab']" (choose_word_length 2) ["a";"ab"] ["ab"] (pp_list i) ( = );
  test "choose 3 ['a';'ab'] is []" (choose_word_length 3) ["a";"ab"] [] (pp_list i) ( = );
]
let load_tests = List.flatten [parse_dict_tests;choose_word_length_tests]
 

(*****************************************************************)
(* tests for the HintEngine module *)
(*****************************************************************)

let yes_engine_st0 = init_engine "yes"
let alphabet = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k";
"l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"]

let yes_engine_st1 =
match get_hint 1 yes_engine_st0 with
| (_, e) -> match get_hint 1 e with
| (_, e) -> match get_hint 1 e with
| (_, e) -> match get_hint 1 e with
| (_, e) -> e

(* remove y, e, s *)
let alphabet_3rm = ["a"; "b"; "c"; "d"; "f"; "g"; "h"; "i"; "j"; "k";
"l"; "m"; "n"; "o"; "p"; "q"; "r"; "t"; "u"; "v"; "w"; "x"; "z"]

let yes_engine_st2 = add_guess yes_engine_st1 "yea"

(* remove y, e, s, a *)
let alphabet_4rm = ["b"; "c"; "d"; "f"; "g"; "h"; "i"; "j"; "k";
"l"; "m"; "n"; "o"; "p"; "q"; "r"; "t"; "u"; "v"; "w"; "x"; "z"]

let hint_tests = [
  (* state 0 - init *)
  test2 "initialized engine with answer \"yes\" contains the correct answer"
  (yes_engine_st0 |> correct_word) "yes" (fun x -> x) ( = );
  test2 "initialized engine with answer \"yes\" has an unguessed keyboard of all 26 letters"
  (yes_engine_st0 |> unguessed_letters) alphabet (pp_list i) ( cmp_list );
  test2 "initialized engine with answer \"yes\" has a grey keyboard of 0 letters"
  (yes_engine_st0 |> grey_letters) [] (pp_list i) ( cmp_list );
  test2 "initialized engine with answer \"yes\" has a yellow keyboard of 0 letters"
  (yes_engine_st0 |> yellow_letters) [] (pp_list i) ( cmp_list );
  test2 "initialized engine with answer \"yes\" has a green keyboard of 0 letters"
  (yes_engine_st0 |> green_letters) [] (pp_list i) ( cmp_list );

  (* state 1 - get three yellow hints from new game with answer "yes" *)
  test2 "taking 3 yellow hints from engine with answer \"yes\" should have
  unguessed keyboard containing all letters but \"y\", \"e\", and \"s\""
  (yes_engine_st1 |> unguessed_letters) alphabet_3rm (pp_list i) ( cmp_list );
  test2 "taking 3 yellow hints from engine with answer \"yes\" should have
  empty grey keyboard "
  (yes_engine_st1 |> grey_letters) [] (pp_list i) ( cmp_list );
  test2 "taking 3 yellow hints from engine with answer \"yes\" should have
  yellow keyboard containing \"y\", \"e\", and \"s\""
  (yes_engine_st1 |> yellow_letters) ["y"; "e"; "s"] (pp_list i) ( cmp_list );
  test2 "taking 3 yellow hints from engine with answer \"yes\" should have
  empty green keyboard"
  (yes_engine_st1 |> green_letters) [] (pp_list i) ( cmp_list );

  (* state 2 - guess "yea"*)
  test2 "guessing \"yea\" after taking 3 yellow hints from engine with answer
  \"yes\" should have unguessed keyboard containing all letters but \"y\",
  \"e\", \"s\", and \"a\""
  (yes_engine_st2 |> unguessed_letters) alphabet_4rm (pp_list i) ( cmp_list );
  test2 "guessing \"yea\" after taking 3 yellow hints from engine with answer
  \"yes\" should have grey keyboard containing \"a\""
  (yes_engine_st2 |> grey_letters) ["a"] (pp_list i) ( cmp_list );
  test2 "guessing \"yea\" after taking 3 yellow hints from engine with answer
  \"yes\" should have yellow keyboard containing \"s\""
  (yes_engine_st2 |> yellow_letters) ["s"] (pp_list i) ( cmp_list );
  test2 "guessing \"yea\" after taking 3 yellow hints from engine with answer
  \"yes\" should have green keyboard containing \"y\" and \"e\""
  (yes_engine_st2 |> green_letters) ["y"; "e"] (pp_list i) ( cmp_list );
]


(*****************************************************************)

let suite =
  "test suite for Wordle 2.0"
  >::: List.flatten [ processor_tests; load_tests; hint_tests]

let _ = run_test_tt_main suite