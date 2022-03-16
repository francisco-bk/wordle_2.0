open Game
open Processor
open Randpick
open Load 

let dict = 5 |> load |> dict_lst 
(** [dict] currently stores the five letter word dictionary, which is of type
string list*)

let correct_word = pick dict 
(** [correct_word] is the correct word *)

(** [naive_processor a] Temporary word processor, to be replaced by functions from src files*)
let naive_processor a = 
  a = correct_word

let print_color_letter c_tuple = match c_tuple with
| (c, 0) -> print_string c
| (c, 1) -> ANSITerminal.print_string [ ANSITerminal.yellow ] c;
| (c, 2) -> ANSITerminal.print_string [ ANSITerminal.green ] c;
| _ -> ()

let rec print_word colored_word = 
  match colored_word with
  | [] -> ()
  | h :: t -> print_color_letter h;
  print_word t

(** [in_check str] check if the str is a valid word by comparing it to dict*)
let in_check str :bool=
  in_dict dict str 


let print_history guess =
  let colored_guess = Processor.colorize_guess correct_word guess in
  print_word colored_guess

(** [end_screen ()] represents the state after the game ends. *)
let end_screen () =
  print_endline "\n\nCongratulations! You have guessed the correct word!";
  print_endline "Score: (To be implemented)"

(** [play ()] represents the in-game state. *)
let rec play () =
  print_string "\nEnter your guess: ";
  let input = read_line () in
  if (in_check input) then (
  let output = input |> naive_processor in
  match output with
  | true -> end_screen ()
  | false ->
    print_endline "\n\nIncorrect!";
    print_history input;
    play () )
  else (print_endline (input^" Is not a valid word");
  play())

(** [start ()] represents the pre-game state. *)
    let start () =
  print_endline "Welcome to Wordle 2.0!";
  print_endline "Guess the five-letter word.";
  print_endline correct_word;
  play()

let () = start ()
