open OUnit2
open Game
open Processor
open Load
open HintEngine
open Leaderboard

(*****************************************************************)
(* TEST PLAN *)
(*****************************************************************)
(* [Main.ml]: Manually tested through the terminal. Since [Main] handles
   the interface between the game and the user, it is intuitive to test
   this by playtesting the game using [make play] and checking that the
   feedback is as expected.

   [Leaderboard.ml]: Manually tested through the terminal. The
   leaderboard is printed after a game is complete in [make play]. Thus,
   we are able to verify its correctness by playtesting the game and
   checking that the leaderboard updates correctly. We also used
   automatic OUnit testing to guarantee that the leaderboard formats
   correctly using black box testing with boundary and standard cases.

   [Load.ml]: Manually tested through the terminal. [Load.ml] provides
   the dictionary that the word to be guessed is pulled from during a
   game session. We are able to verify correctness by playing through
   the game and ensuring that the gameboard selects an answer with the
   expected number of letters. We also used automatic OUnit black-box
   testing with boundary and standard cases to ensure that the
   formatting of the loaded dictionary is correct.

   [RandPick.ml]: Manually tested through the terminal. [RandPick.ml]
   chooses a specific word to be guessed by the player during the game
   session from a dictionary. We are able to verify correctness by
   playing through the game and ensuring that the gameboard selects
   different words in different sessions.

   [HintEngine.ml]: Automatically tested by OUnit. The test cases were
   developed via glass box testing. The hints are generated based off of
   a keyboard stored in [HintEngine.t]. We are able to verify
   correctness this way because if we can test that each of the four
   keyboards (unguessed, grey, yellow, and green) store the letters
   correctly when updated, then [HintEngine.t] will output valid hints
   without giving ones with unhelpful information.

   [Processor.ml]: Automatically tested by OUnit. Black box testing was
   used to generate test cases. Processor's job is to color the guesses
   bases on the answer in the wordle game state, we are able to prove
   correctness since there are so many different possible cases for the
   array output, black box testing is the most efficient way to cover
   the most possible cases efficiently. Both general cases with little
   in common were used, and boundary cases to make sure it works for all
   possible works and lengths. *)

(*****************************************************************)
(* Helper functions to construct tests *)
(*****************************************************************)

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
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
    testing function [f] with input [input] (NOTE: to use multiple
    inputs, put the other inputs with the function like so (f a) b),
    [ex_output] represents the expected output, printer takes the return
    type of the function to string, and [cmp] is the comparator to
    compare the results *)
let test (name : string) f input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (f input) ~cmp ~printer

(** [test2 name input ex_output printer cmp] makes a test named [name]
    to verify [input] against [ex_output]. This combines f and input
    from [test]. *)
let test2 (name : string) input expected_output printer cmp : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output input ~cmp ~printer

(** [test_exc name exc input] makes a test named [name] to verify that
    [input] raises exception [exc]. Requires: [input] is inside an
    anonymous function *)
let test_exc (name : string) exc input : test =
  name >:: fun _ -> assert_raises exc input

(*****************************************************************)
(* tests for the Processor module *)
(*****************************************************************)
let processor_tests =
  [
    (* Testing format *)
    test "format '   hello'" Processor.format "   hello" "HELLO"
      (fun x -> x)
      ( = );
    test "format ''" Processor.format "" "" (fun x -> x) ( = );
    test "format '   hElLo  '" Processor.format "   hElLo  " "HELLO"
      (fun x -> x)
      ( = );
    test "format 'HELLO'" Processor.format "HELLO" "HELLO"
      (fun x -> x)
      ( = );
    (* Testing are_equal *)
    test "are_equal 'hello' and 'hello'" (are_equal "hello") "hello"
      true string_of_bool ( = );
    test "are_equal 'aaaaa' and 'hello'" (are_equal "aaaaa") "hello"
      false string_of_bool ( = );
    (* Testing in_dict *)
    test "in_dict ['bbbbb', 'aaaaa'] and 'aaaaa'"
      (in_dict [ "bbbbb"; "aaaaa" ])
      "aaaaa" true string_of_bool ( = );
    test "in_dict [] and 'aaaaa'" (in_dict []) "aaaaa" false
      string_of_bool ( = );
    test "in_dict ['aaaaa'] and 'aaaaa'"
      (in_dict [ "aaaaa" ])
      "aaaaa" true string_of_bool ( = );
    (* Testing color_list *)
    test "color_list 'llama' and 'lamal'" (color_list "llama") "lamal"
      [ 2; 1; 1; 1; 1 ] (pp_list Int.to_string) ( = );
    test "color_list 'eevie' and 'etvoe'" (color_list "eevie") "etvoe"
      [ 2; 0; 2; 0; 2 ] (pp_list Int.to_string) ( = );
    test "color_list 'salet' and 'wordy'" (color_list "salet") "wordy"
      [ 0; 0; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "color_list 'baaab' and 'abbba'" (color_list "baaab") "abbba"
      [ 1; 1; 1; 0; 1 ] (pp_list Int.to_string) ( = );
    test "color_list 'babbb' and 'bbbbb'" (color_list "babbb") "bbbbb"
      [ 2; 0; 2; 2; 2 ] (pp_list Int.to_string) ( = );
    test "color_list 'wordy' and 'rdrpp'" (color_list "wordy") "rdrpp"
      [ 0; 1; 2; 0; 0 ] (pp_list Int.to_string) ( = );
    test "color_list 'helloworld' and 'ajdhouslof'"
      (color_list "helloworld")
      "ajdhouslof"
      [ 0; 0; 1; 1; 2; 0; 0; 1; 1; 0 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'hello' and 'lolal'" (color_list "hello")
      "lolal" [ 1; 1; 2; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'bcnid' and 'hasuh'" (color_list "bcnid")
      "hasuh" [ 0; 0; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess '' and ''" (color_list "") "" []
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'baksjghsif' and 'hfiifodpsp'"
      (color_list "baksjghsif")
      "hfiifodpsp"
      [ 1; 1; 1; 0; 0; 0; 0; 0; 1; 0 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'goods' and 'books'" (color_list "goods")
      "books" [ 0; 2; 2; 0; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'interact' and 'database'"
      (color_list "interact") "database"
      [ 0; 0; 2; 0; 0; 2; 0; 1 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'agge' and 'podd'" (color_list "agge") "podd"
      [ 0; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'chicken' and 'baksasf'" (color_list "chicken")
      "baksasf" [ 0; 0; 1; 0; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'asdd' and 'afsf'" (color_list "asdd") "afsf"
      [ 2; 0; 1; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'afafasdd' and 'aadsafsf'"
      (color_list "afafasdd") "aadsafsf"
      [ 2; 1; 1; 1; 2; 1; 0; 1 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'zxops' and 'sfiso'" (color_list "zxops")
      "sfiso" [ 1; 0; 0; 0; 1 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'portal' and 'cockle'" (color_list "portal")
      "cockle" [ 0; 2; 0; 0; 1; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'as' and 'ff'" (color_list "as") "ff" [ 0; 0 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'hao' and 'sdg'" (color_list "hao") "sdg"
      [ 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'qwiug' and 'qggif'" (color_list "qwiug")
      "qggif" [ 2; 1; 0; 1; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'asdfghjkll' and 'aslfkgcksd'"
      (color_list "asdfghjkll")
      "aslfkgcksd"
      [ 2; 2; 1; 2; 0; 1; 0; 2; 0; 1 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'lmvfp' and 'imvfi'" (color_list "lmvfp")
      "imvfi" [ 0; 2; 2; 2; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'basud' and 'sdsad'" (color_list "basud")
      "sdsad" [ 0; 0; 2; 1; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'finoe' and 'fions'" (color_list "finoe")
      "fions" [ 2; 2; 1; 1; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'adddd' and 'gsddd'" (color_list "adddd")
      "gsddd" [ 0; 0; 2; 2; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'dsino' and 'casin'" (color_list "dsino")
      "casin" [ 0; 0; 1; 1; 1 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'dsasd' and 'async'" (color_list "dsasd")
      "async" [ 1; 2; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'hjfksjfhsjak' and 'akfjskifowef'"
      (color_list "hjfksjfhsjak")
      "akfjskifowef"
      [ 1; 1; 2; 1; 2; 1; 0; 1; 0; 0; 0; 0 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'bhds' and 'asid'" (color_list "bhds") "asid"
      [ 0; 1; 0; 1 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'vujgi' and 'funci'" (color_list "vujgi")
      "funci" [ 0; 2; 0; 0; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'gr' and 'gu'" (color_list "gr") "gu" [ 2; 0 ]
      (pp_list Int.to_string) ( = );
    test "colorize_guess 'testing' and 'testing'" (color_list "testing")
      "testing" [ 2; 2; 2; 2; 2; 2; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'wordy' and 'tubes'" (color_list "wordy")
      "tubes" [ 0; 0; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'fling' and 'crane'" (color_list "fling")
      "crane" [ 0; 0; 0; 2; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'plane' and 'stroe'" (color_list "plane")
      "stroe" [ 0; 0; 0; 0; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'helps' and 'money'" (color_list "helps")
      "money" [ 0; 0; 0; 1; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'jen' and 'ben'" (color_list "jen") "ben"
      [ 0; 2; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'utop' and 'tops'" (color_list "utop") "tops"
      [ 1; 1; 1; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'boble' and 'gogle'" (color_list "boble")
      "gogle" [ 0; 2; 0; 2; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'array' and 'hello'" (color_list "array")
      "hello" [ 0; 0; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'cindy' and 'franc'" (color_list "cindy")
      "franc" [ 0; 0; 0; 1; 1 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'aoind' and 'guiwu'" (color_list "aoind")
      "guiwu" [ 0; 0; 2; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'finof' and 'dinos'" (color_list "finof")
      "dinos" [ 0; 2; 2; 2; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'woufh' and 'fownf'" (color_list "woufh")
      "fownf" [ 1; 2; 1; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'champ' and 'funel'" (color_list "champ")
      "funel" [ 0; 0; 0; 0; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'snale' and 'snake'" (color_list "snale")
      "snake" [ 2; 2; 2; 0; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'sinf' and 'sifn'" (color_list "sinf") "sifn"
      [ 2; 2; 1; 1 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'asi' and 'fio'" (color_list "asi") "fio"
      [ 0; 1; 0 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'since' and 'loops'" (color_list "since")
      "loops" [ 0; 0; 0; 0; 1 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'grey' and 'gray'" (color_list "grey") "gray"
      [ 2; 2; 0; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'grey' and 'gray'" (color_list "grey") "gray"
      [ 2; 2; 0; 2 ] (pp_list Int.to_string) ( = );
    test "colorize_guess 'soind' and 'sdima'" (color_list "soind")
      "sdima" [ 2; 1; 2; 0; 0 ] (pp_list Int.to_string) ( = );
  ]

(*****************************************************************)
(* tests for the Load module *)
(*****************************************************************)

let i a = a

let parse_dict_tests =
  [
    test "parse empty is empty" parse_dict [] [] (pp_list i) ( = );
    test "parse ['a'] is ['a']" parse_dict [ "a" ] [ "a" ] (pp_list i)
      ( = );
    test "parse [' a '] is ['a']" parse_dict [ " a " ] [ "a" ]
      (pp_list i) ( = );
    test "parse [' a'] is ['a']" parse_dict [ " a" ] [ "a" ] (pp_list i)
      ( = );
    test "parse ['a';'b'] is ['a';'b']" parse_dict [ "a"; "b" ]
      [ "a"; "b" ] (pp_list i) ( = );
    test "parse ['a';' b '] is ['a';'b']" parse_dict [ "a"; " b " ]
      [ "a"; "b" ] (pp_list i) ( = );
    test "parse test" parse_dict [ "a      "; " b " ] [ "a"; "b" ]
      (pp_list i) ( = );
    test "parse test" parse_dict [ "      a"; " b " ] [ "a"; "b" ]
      (pp_list i) ( = );
    test "parse test" parse_dict
      [ "a    "; " b       " ]
      [ "a"; "b" ] (pp_list i) ( = );
    test "parse test" parse_dict [ "a a"; " b " ] [ "a a"; "b" ]
      (pp_list i) ( = );
    test "parse test" parse_dict [ "a   "; " b b" ] [ "a"; "b b" ]
      (pp_list i) ( = );
    test "parse test" parse_dict [ "a a a"; " b " ] [ "a a a"; "b" ]
      (pp_list i) ( = );
    test "parse test" parse_dict [ "a a a a"; " b " ] [ "a a a a"; "b" ]
      (pp_list i) ( = );
    test "parse test" parse_dict
      [ "a a a a       "; " b " ]
      [ "a a a a"; "b" ] (pp_list i) ( = );
    test "parse test" parse_dict [ ""; " b " ] [ ""; "b" ] (pp_list i)
      ( = );
    test "parse test" parse_dict [ "1   1    "; " b " ] [ "1   1"; "b" ]
      (pp_list i) ( = );
  ]

let choose_word_length_tests =
  [
    test "choose empty is empty" (choose_word_length 5) [] []
      (pp_list i) ( = );
    test "choose 1 ['a'] is ['a']" (choose_word_length 1) [ "a" ]
      [ "a" ] (pp_list i) ( = );
    test "choose 1 ['ab'] is []" (choose_word_length 1) [ "ab" ] []
      (pp_list i) ( = );
    test "choose 2 ['ab'] is ['ab']" (choose_word_length 2) [ "ab" ]
      [ "ab" ] (pp_list i) ( = );
    test "choose 1 ['a';'ab'] is ['a']" (choose_word_length 1)
      [ "a"; "ab" ] [ "a" ] (pp_list i) ( = );
    test "choose 2 ['a';'ab'] is ['ab']" (choose_word_length 2)
      [ "a"; "ab" ] [ "ab" ] (pp_list i) ( = );
    test "choose 3 ['a';'ab'] is []" (choose_word_length 3)
      [ "a"; "ab" ] [] (pp_list i) ( = );
    test "choose 4 ['a';'ab'] is []" (choose_word_length 4)
      [ "a"; "ab" ] [] (pp_list i) ( = );
    test "choose 5 ['a';'ab'] is []" (choose_word_length 5)
      [ "a"; "ab" ] [] (pp_list i) ( = );
    test "choose 1 ['a';'b'] is ['a';'b']" (choose_word_length 1)
      [ "a"; "b" ] [ "a"; "b" ] (pp_list i) ( = );
    test "choose 2 ['a';'b'] is []" (choose_word_length 2) [ "a"; "b" ]
      [] (pp_list i) ( = );
    test "choose 1 ['';''] is []" (choose_word_length 1) [ ""; "" ] []
      (pp_list i) ( = );
    test "choose 0 ['';''] is ['';'']" (choose_word_length 0) [ ""; "" ]
      [ ""; "" ] (pp_list i) ( = );
    test "choose 0 ['a';''] is ['']" (choose_word_length 0) [ "a"; "" ]
      [ "" ] (pp_list i) ( = );
    test "choose 1000 ['a';'b'] is []"
      (choose_word_length 1000)
      [ "a"; "b" ] [] (pp_list i) ( = );
  ]

let str_to_lst_tests =
  [
    test "str_to_lst test" str_to_lst "a,a" [ "a"; "a" ] (pp_list i)
      ( = );
    test "str_to_lst test" str_to_lst "a,a,a" [ "a"; "a"; "a" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "" [ "" ] (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "a,a,a,a" [ "a"; "a"; "a"; "a" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "a,b" [ "a"; "b" ] (pp_list i)
      ( = );
    test "str_to_lst test" str_to_lst "a" [ "a" ] (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "a," [ "a"; "" ] (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "," [ ""; "" ] (pp_list i) ( = );
    test "str_to_lst test" str_to_lst ",," [ ""; ""; "" ] (pp_list i)
      ( = );
    test "str_to_lst test" str_to_lst "a,a,b" [ "a"; "a"; "b" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "a,a," [ "a"; "a"; "" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst ",a" [ ""; "a" ] (pp_list i) ( = );
    test "str_to_lst test" str_to_lst ",a,a," [ ""; "a"; "a"; "" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "   ,a,a, " [ ""; "a"; "a"; "" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst ",a,a  ," [ ""; "a"; "a"; "" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst ",    a,a," [ ""; "a"; "a"; "" ]
      (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "    ,a    ,a    ,   "
      [ ""; "a"; "a"; "" ] (pp_list i) ( = );
    test "str_to_lst test" str_to_lst ";" [ ";" ] (pp_list i) ( = );
    test "str_to_lst test" str_to_lst "???" [ "???" ] (pp_list i) ( = );
  ]

let load_tests =
  List.flatten
    [ parse_dict_tests; choose_word_length_tests; str_to_lst_tests ]

(*****************************************************************)
(* tests for the Leaderboard module *)
(*****************************************************************)

let check_board_tests =
  [
    test "check_board test" check_board [] false string_of_bool ( = );
    test "check_board test" check_board [ "" ] true string_of_bool ( = );
    test "check_board test" check_board [ 1 ] true string_of_bool ( = );
    test "check_board test" check_board [ "hey"; "hey" ] true
      string_of_bool ( = );
    test "check_board test" check_board [ 1; 2 ] true string_of_bool
      ( = );
    test "check_board test" check_board [ [] ] true string_of_bool ( = );
    test "check_board test" check_board [ []; []; [] ] true
      string_of_bool ( = );
    test "check_board test" check_board [ 1 ] true string_of_bool ( = );
    test "check_board test" check_board [ () ] true string_of_bool ( = );
    test "check_board test" check_board [ (); () ] true string_of_bool
      ( = );
  ]

let pickff_test
    (name : string)
    (lst : string list)
    (expected_output : (string * int) list) : test =
  name >:: fun _ -> assert_equal expected_output (pick_first_five lst)

let pickff_tests =
  [
    pickff_test "pick_first_five test" [ "a "; "1" ] [ ("a ", 1) ];
    pickff_test "pick_first_five test with only one pair" [ "aa "; "1" ]
      [ ("aa ", 1) ];
    pickff_test "pick_first_five test with only one pair" [ "a "; "2" ]
      [ ("a ", 2) ];
    pickff_test "pick_first_five test with only one pair" [ "a "; "12" ]
      [ ("a ", 12) ];
    pickff_test "pick_first_five test with only one pair" [ "ab "; "1" ]
      [ ("ab ", 1) ];
    pickff_test "pick_first_five test with only one pair"
      [ "a "; "-1000" ]
      [ ("a ", -1000) ];
    pickff_test
      "pick_first_five test with two pairs, each has a unique trait"
      [ "a "; "1"; "b "; "2" ]
      [ ("b ", 2); ("a ", 1) ];
    pickff_test
      "pick_first_five test with two pairs, each has a unique trait"
      [ "a "; "2"; "b "; "3" ]
      [ ("b ", 3); ("a ", 2) ];
    pickff_test
      "pick_first_five test with two pairs, each has a unique trait"
      [ "b "; "-10"; "a "; "2" ]
      [ ("a ", 2); ("b ", -10) ];
    pickff_test
      "pick_first_five test with two pairs, each has a unique trait"
      [ "a "; "99"; "b "; "100" ]
      [ ("b ", 100); ("a ", 99) ];
    pickff_test
      "pick_first_five test with two pairs, each has a unique trait"
      [ "a "; "-10"; "b "; "-9" ]
      [ ("b ", -9); ("a ", -10) ];
  ]

let pickff_tests' =
  [
    pickff_test
      "pick_first_five test with two pairs, each has a unique trait"
      [ "a "; "11"; "b "; "20" ]
      [ ("b ", 20); ("a ", 11) ];
    pickff_test "pick_first_five test with two pairs, order is random"
      [ "a "; "2"; "b "; "1" ]
      [ ("a ", 2); ("b ", 1) ];
    pickff_test "pick_first_five test with two pairs, order is random"
      [ "a "; "3"; "b "; "1" ]
      [ ("a ", 3); ("b ", 1) ];
    pickff_test "pick_first_five test with two pairs, order is random"
      [ "a "; "2"; "b "; "-1" ]
      [ ("a ", 2); ("b ", -1) ];
    pickff_test
      "pick_first_five test with multiple pairs, order is random"
      [ "a "; "2"; "b "; "1"; "c "; "3" ]
      [ ("c ", 3); ("a ", 2); ("b ", 1) ];
    pickff_test
      "pick_first_five test with multiple pairs, order is random"
      [ "a "; "1"; "b "; "2"; "c "; "3" ]
      [ ("c ", 3); ("b ", 2); ("a ", 1) ];
    pickff_test
      "pick_first_five test with multiple pairs, order is random"
      [ "a "; "200"; "b "; "1"; "c "; "3" ]
      [ ("a ", 200); ("c ", 3); ("b ", 1) ];
    pickff_test
      "pick_first_five test with multiple pairs, order is random"
      [ "a "; "-22"; "b "; "1"; "c "; "3" ]
      [ ("c ", 3); ("b ", 1); ("a ", -22) ];
    pickff_test
      "pick_first_five test with multiple pairs, order is random"
      [ "a "; "0"; "b "; "-11"; "c "; "-10" ]
      [ ("a ", 0); ("c ", -10); ("b ", -11) ];
  ]

let format_test
    (name : string)
    (lst : string list)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Leaderboard.format lst)

let format_tests =
  [
    format_test
      "format test with just one pair, each with a unique trait"
      [ "a "; "1" ] "a ,1;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "a "; "2" ] "a ,2;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "b "; "1" ] "b ,1;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "aba "; "1" ] "aba ,1;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "aa "; "1" ] "aa ,1;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "a "; "100" ] "a ,100;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "a "; "-100" ] "a ,-100;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "a1 "; "0" ] "a1 ,0;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "a1 "; "1" ] "a1 ,1;";
    format_test
      "format test with just one pair, each with a unique trait"
      [ "a100 "; "1" ] "a100 ,1;";
  ]

let format_tests' =
  [
    format_test
      "format test with two pairs, order is random and values are \
       representative1"
      [ "a "; "1"; "b "; "2" ]
      "b ,2;a ,1;";
    format_test
      "format test with two pairs, order is random and values are \
       representative2"
      [ "a "; "300"; "b "; "200" ]
      "b ,200;a ,300;";
    format_test
      "format test with two pairs, order is random and values are \
       representative3"
      [ "ab "; "1"; "ab "; "2" ]
      "ab ,2;ab ,1;";
    format_test
      "format test with two pairs, order is random and values are \
       representative4"
      [ "ab "; "2"; "ba "; "1" ]
      "ba ,1;ab ,2;";
    format_test
      "format test with two pairs, order is random and values are \
       representative5"
      [ "a "; "2"; "b "; "2" ]
      "b ,2;a ,2;";
    format_test
      "format test with two pairs, order is random and values are \
       representative6"
      [ "a "; "-1"; "b "; "20" ]
      "b ,20;a ,-1;";
    format_test
      "format test with two pairs, order is random and values are \
       representative7"
      [ "aaaa "; "1"; "aaaaa "; "2" ]
      "aaaaa ,2;aaaa ,1;";
    format_test
      "format test with two pairs, order is random and values are \
       representative8"
      [ "a3a "; "1"; "a3a "; "2" ]
      "a3a ,2;a3a ,1;";
    format_test
      "format test with two pairs, order is random and values are \
       representative9"
      [ "a "; "1000"; "b "; "2000" ]
      "b ,2000;a ,1000;";
    format_test
      "format test with two pairs, order is random and values are \
       representative10"
      [ "a "; "-1"; "b "; "-2" ]
      "b ,-2;a ,-1;";
  ]

let leaderboard_tests =
  List.flatten
    [
      check_board_tests;
      pickff_tests;
      pickff_tests';
      format_tests;
      format_tests';
    ]
(*****************************************************************)
(* tests for the HintEngine module *)
(*****************************************************************)

let alphabet =
  [
    "a";
    "b";
    "c";
    "d";
    "e";
    "f";
    "g";
    "h";
    "i";
    "j";
    "k";
    "l";
    "m";
    "n";
    "o";
    "p";
    "q";
    "r";
    "s";
    "t";
    "u";
    "v";
    "w";
    "x";
    "y";
    "z";
  ]

(**** Helpers for Test Run 1 ****)
let yes_engine_st0 = init_engine "yes"

let yes_engine_st1 =
  match get_hint 1 yes_engine_st0 with
  | _, e -> (
      match get_hint 1 e with
      | _, e -> (
          match get_hint 1 e with
          | _, e -> (
              match get_hint 1 e with
              | _, e -> e)))

(* remove y, e, s *)
let alphabet_3rm_tr1 =
  List.filter (fun l -> l <> "y" && l <> "e" && l <> "s") alphabet

let yes_engine_st2 = add_guess yes_engine_st1 "yea"

(* remove y, e, s, a *)
let alphabet_4rm_tr1 = List.filter (fun l -> l <> "a") alphabet_3rm_tr1
let yes_engine_st3 = add_guess yes_engine_st1 "yep"

(* remove y, e, s, a, p *)
let alphabet_5rm_tr1 = List.filter (fun l -> l <> "p") alphabet_4rm_tr1

(**** Helpers for Test Run 2 ****)
let bee_engine_st0 = init_engine "bee"
let bee_engine_st1 = add_guess bee_engine_st0 "zxcv"

(* remove z, x, c, v *)
let alphabet_4rm_tr2 =
  List.filter
    (fun l -> l <> "z" && l <> "x" && l <> "c" && l <> "v")
    alphabet

let bee_engine_st2 =
  match get_hint 0 bee_engine_st1 with
  | _, e -> e

let bee_engine_st3 = add_guess bee_engine_st2 "eye"

let bee_engine_st4 =
  match get_hint 1 bee_engine_st3 with
  | _, e -> e

let bee_engine_st5 =
  match get_hint 1 bee_engine_st4 with
  | _, e -> e

let hint_bee_engine_st5 =
  match get_hint 1 bee_engine_st4 with
  | h, _ -> h

let hint_tests =
  [
    (*************************)
    (* Test-Run 1: Answer has all unique letters. *)
    (*************************)
    (* Test-Run 1 state 0 - init *)
    test2
      "initialized engine with answer \"yes\" contains the correct \
       answer"
      (yes_engine_st0 |> correct_word)
      "yes"
      (fun x -> x)
      ( = );
    test2
      "initialized engine with answer \"yes\" has an unguessed \
       keyboard of all\n\
      \  26 letters"
      (yes_engine_st0 |> unguessed_letters)
      alphabet (pp_list i) cmp_list;
    test2
      "initialized engine with answer \"yes\" has a grey keyboard of 0 \
       letters"
      (yes_engine_st0 |> grey_letters)
      [] (pp_list i) cmp_list;
    test2
      "initialized engine with answer \"yes\" has a yellow keyboard of \
       0 letters"
      (yes_engine_st0 |> yellow_letters)
      [] (pp_list i) cmp_list;
    test2
      "initialized engine with answer \"yes\" has a green keyboard of \
       0 letters"
      (yes_engine_st0 |> green_letters)
      [] (pp_list i) cmp_list;
    (* Test-Run 1 state 1 - get three yellow hints from new game with
       answer "yes" *)
    test2
      "taking 3 yellow hints from engine with answer \"yes\" should have\n\
      \  unguessed keyboard containing all letters but \"y\", \"e\", \
       and \"s\""
      (yes_engine_st1 |> unguessed_letters)
      alphabet_3rm_tr1 (pp_list i) cmp_list;
    test2
      "taking 3 yellow hints from engine with answer \"yes\" should have\n\
      \  empty grey keyboard "
      (yes_engine_st1 |> grey_letters)
      [] (pp_list i) cmp_list;
    test2
      "taking 3 yellow hints from engine with answer \"yes\" should have\n\
      \  yellow keyboard containing \"y\", \"e\", and \"s\""
      (yes_engine_st1 |> yellow_letters)
      [ "y"; "e"; "s" ] (pp_list i) cmp_list;
    test2
      "taking 3 yellow hints from engine with answer \"yes\" should have\n\
      \  empty green keyboard"
      (yes_engine_st1 |> green_letters)
      [] (pp_list i) cmp_list;
    (* Test-Run 1 state 2 - guess "yea" *)
    test2
      "guessing \"yea\" after taking 3 yellow hints from engine with \
       answer \"yes\" should have unguessed keyboard containing all \
       letters but \"y\", \"e\", \"s\", and \"a\""
      (yes_engine_st2 |> unguessed_letters)
      alphabet_4rm_tr1 (pp_list i) cmp_list;
    test2
      "guessing \"yea\" after taking 3 yellow hints from engine with \
       answer \"yes\" should have grey keyboard containing \"a\""
      (yes_engine_st2 |> grey_letters)
      [ "a" ] (pp_list i) cmp_list;
    test2
      "guessing \"yea\" after taking 3 yellow hints from engine with \
       answer \"yes\" should have yellow keyboard containing \"s\""
      (yes_engine_st2 |> yellow_letters)
      [ "s" ] (pp_list i) cmp_list;
    test2
      "guessing \"yea\" after taking 3 yellow hints from engine with \
       answer \"yes\" should have green keyboard containing \"y\" and \
       \"e\""
      (yes_engine_st2 |> green_letters)
      [ "y"; "e" ] (pp_list i) cmp_list;
    (*************************)
    (* Test-Run 2: Answer has two non-unique letters. *)
    (*************************)
    (* Test-Run 2 state 0 - init *)
    test2
      "initial engine with answer \"bee\" contains the correct answer"
      (bee_engine_st0 |> correct_word)
      "bee"
      (fun x -> x)
      ( = );
    test2
      "initial engine with answer \"bee\" has an unguessed keyboard of \
       all 26 letters"
      (bee_engine_st0 |> unguessed_letters)
      alphabet (pp_list i) cmp_list;
    test2
      "initial engine with answer \"bee\" has a grey keyboard of 0 \
       letters"
      (bee_engine_st0 |> grey_letters)
      [] (pp_list i) cmp_list;
    test2
      "initial engine with answer \"bee\" has a yellow keyboard of 0 \
       letters"
      (bee_engine_st0 |> yellow_letters)
      [] (pp_list i) cmp_list;
    test2
      "initial engine with answer \"bee\" has a green keyboard of 0 \
       letters"
      (bee_engine_st0 |> green_letters)
      [] (pp_list i) cmp_list;
    (* Test-Run 2 state 1 - guess "zxcv" *)
    test2
      "guessing \"zxcv\" in engine with answer \"bee\" should have\n\
      \  unguessed keyboard containing all letters but \"z\", \"x\", \
       \"c\" and \"v\""
      (bee_engine_st1 |> unguessed_letters)
      alphabet_4rm_tr2 (pp_list i) cmp_list;
    test2
      "guessing \"zxcv\" in engine with answer \"bee\" should have\n\
      \  grey keyboard containing \"z\", \"x\", \"c\" and \"v\""
      (bee_engine_st1 |> grey_letters)
      [ "z"; "x"; "c"; "v" ] (pp_list i) cmp_list;
    test2
      "guessing \"zxcv\" in engine with answer \"bee\" should have\n\
      \  empty yellow keyboard"
      (bee_engine_st1 |> yellow_letters)
      [] (pp_list i) cmp_list;
    test2
      "guessing \"zxcv\" in engine with answer \"bee\" should have\n\
      \  empty green keyboard"
      (bee_engine_st1 |> green_letters)
      [] (pp_list i) cmp_list;
    (* Test-Run 2 state 2 - get grey hint *)
    test2
      "getting grey hint after guessing four grey letters in engine with\n\
      \  answer \"bee\" should have unguessed keyboard of size 21"
      (bee_engine_st2 |> unguessed_letters |> List.length)
      21 string_of_int ( = );
    test2
      "getting grey hint after guessing \"zxcv\" in engine with answer\n\
      \  \"bee\" should have grey keyboard of size 5"
      (bee_engine_st2 |> grey_letters |> List.length)
      5 string_of_int ( = );
    test2
      "After getting grey hint after guessing four grey letters in \
       engine with answer \"bee\", combining the grey and unguessed \
       keyboard should result in the full alphabet."
      ((bee_engine_st2 |> grey_letters)
      @ (bee_engine_st2 |> unguessed_letters))
      alphabet (pp_list i) cmp_list;
    test2
      "getting grey hint after getting four grey letters in engine with\n\
      \  answer \"bee\" should have empty yellow keyboard"
      (bee_engine_st2 |> yellow_letters)
      [] (pp_list i) cmp_list;
    test2
      "getting grey hint after getting four grey letters in engine\n\
      \  with answer \"bee\" should have empty green keyboard"
      (bee_engine_st2 |> green_letters)
      [] (pp_list i) cmp_list;
    (* Test-Run 2 state 3 - guess "eye" *)
    test2
      "guessing \"eye\" after getting grey hint and guessing four grey \
       letters in engine with answer \"bee\" should have unguessed \
       keyboard of size <=21"
      (bee_engine_st3 |> unguessed_letters |> List.length)
      21 string_of_int ( >= );
    test2
      "guessing \"eye\" after getting grey hint and guessing four grey \
       letters in engine with answer \"bee\" should have grey keyboard \
       of size >= 5"
      (bee_engine_st3 |> grey_letters |> List.length)
      5 string_of_int ( <= );
    test2
      "guessing \"eye\" after getting grey hint and guessing four grey \
       letters in engine with answer \"bee\" should have empty yellow \
       keyboard"
      (bee_engine_st3 |> yellow_letters)
      [] (pp_list i) cmp_list;
    test2
      "guessing \"eye\" after getting grey hint and guessing four grey \
       letters in engine with answer \"bee\" should have empty green \
       keyboard"
      (bee_engine_st3 |> green_letters)
      [ "e" ] (pp_list i) cmp_list;
    (* Test-Run 2 state 4 - get one yellow hint *)
    test2
      "getting one yellow hint after guessing \"eye\", getting grey \
       hint, and guessing four grey letters in engine with answer \
       \"bee\" should have unguessed keyboard 1 smaller than in state \
       3"
      (bee_engine_st4 |> unguessed_letters |> List.length)
      ((bee_engine_st3 |> unguessed_letters |> List.length) - 1)
      string_of_int ( = );
    test2
      "getting one yellow hint after guessing \"eye\", getting grey \
       hint, and guessing four grey letters in engine with answer \
       \"bee\" should have same grey keyboard as in state 3"
      (bee_engine_st4 |> grey_letters)
      (bee_engine_st3 |> grey_letters)
      (pp_list i) cmp_list;
    test2
      "getting one yellow hint after guessing \"eye\", getting grey \
       hint, and guessing four grey letters in engine with answer \
       \"bee\" should have yellow keyboard with \"b\""
      (bee_engine_st4 |> yellow_letters)
      [ "b" ] (pp_list i) cmp_list;
    test2
      "getting one yellow hint after guessing \"eye\", getting grey \
       hint and getting four grey letters in engine with answer \
       \"bee\" should have empty green keyboard"
      (bee_engine_st4 |> green_letters)
      [ "e" ] (pp_list i) cmp_list;
    (* Test-Run 2 state 5 - get another yellow hint *)
    test2
      "getting a yellow hint after getting a yellow hint, guessing \
       \"eye\", getting grey hint and getting four grey letters in \
       engine with answer \"bee\" should have the same unguessed \
       keyboard as state 4"
      (bee_engine_st5 |> unguessed_letters)
      (bee_engine_st4 |> unguessed_letters)
      (pp_list i) cmp_list;
    test2
      "getting a yellow hint after getting a yellow hint, guessing \
       \"eye\", getting grey hint and getting four grey letters in \
       engine with answer \"bee\" should have the same grey keyboard \
       as state 4"
      (bee_engine_st5 |> grey_letters)
      (bee_engine_st4 |> grey_letters)
      (pp_list i) cmp_list;
    test2
      "getting a yellow hint after getting a yellow hint, guessing \
       \"eye\", getting grey hint and getting four grey letters in \
       engine with answer \"bee\" should have the same yellow keyboard \
       as state 4"
      (bee_engine_st5 |> yellow_letters)
      (bee_engine_st4 |> yellow_letters)
      (pp_list i) cmp_list;
    test2
      "getting a yellow hint after getting a yellow hint, guessing \
       \"eye\", getting grey hint and getting four grey letters in \
       engine with answer \"bee\" should have the same green keyboard \
       as state 4"
      (bee_engine_st5 |> green_letters)
      (bee_engine_st4 |> green_letters)
      (pp_list i) cmp_list;
    test2
      "getting a yellow hint after getting a yellow hint, guessing \
       \"eye\", getting grey hint and getting four grey letters in \
       engine with answer \"bee\" should return an invalid hint"
      hint_bee_engine_st5 None
      (fun x ->
        match x with
        | Some _ -> "Some hint"
        | None -> "")
      ( = );
  ]

(*****************************************************************)

let suite =
  "test suite for Wordle 2.0"
  >::: List.flatten
         [ processor_tests; load_tests; hint_tests; leaderboard_tests ]

let _ = run_test_tt_main suite
