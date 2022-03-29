open Game
open Processor
open Randpick
open Load

let rec empty_row column : string =
  if column = 0 then "|" else "|   " ^ empty_row (column - 1)

let rec empty_grid row column : unit = 
  let e_line = empty_row column in 
    if row = 0 then () else ((print_endline e_line); empty_grid (row - 1) column)
    
let print_color_letter c_tuple = match c_tuple with
  | (c, 0) -> print_string c
  | (c, 1) -> ANSITerminal.print_string [ ANSITerminal.yellow ] c;
  | (c, 2) -> ANSITerminal.print_string [ ANSITerminal.green ] c;
  | _ -> ()
    

let rec colored_row column (letters : (string * int) list) : unit =
  if column = 0 then () else match letters with
  | [] -> print_string "|"
  | h :: t -> print_string "| "; print_color_letter h; print_string " ";
    colored_row column t
  
let rec make_grid row column (guesses : ((string * int) list) list) : unit = 
  match guesses with 
  | [] -> empty_grid row column
  | h :: t -> colored_row column h; print_endline ""; make_grid (row - 1) column t

(** [make_game d l g] makes a game with [d] rows representing the difficulty,
[l] letter representing the number of letters, and with [g] guesses representing
the guesses so far.
Precondition : length of guesses is smaller then dif*)
let make_game dif letters guesses : unit =
  print_endline "Wordle 2.0 ( i ) ( l )";
  make_grid dif letters guesses

let dict = 5 |> load |> dict_lst 
(** [dict] currently stores the five letter word dictionary, which is of type
string list*)

let correct_word = pick dict 
(** [correct_word] is the correct word *)

(** [naive_processor a] Temporary word processor, to be replaced by functions from src files*)
let naive_processor a = 
  a = correct_word


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
  print_endline "\nCongratulations! You have guessed the correct word!";
  print_endline "Score: (To be implemented)";
  print_endline ""

(** [play ()] represents the in-game state. *)
let rec play (guesses : ((string*int) list)list) () =
  make_game 6 5 guesses;
  let input = read_line () in
  if (in_check input) then (
  let output = input |> naive_processor in
  match output with
  | true -> make_game 6 5 (guesses @ [(colorize_guess correct_word input)]); end_screen ()
  | false -> play (guesses @ [(colorize_guess correct_word input)]) () )
  else (print_endline (input^" is not a valid word");
  play guesses ())

(** [start ()] represents the pre-game state. *)
    let start () =
  print_endline "Welcome to Wordle 2.0!";
  print_endline "Guess the five-letter word.";
  play [] ()
  
let () = start ()