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

let keyboard = [["q"; "w"; "e"; "r"; "t"; "y"; "u"; "i"; "o"; "p"]; 
  ["a"; "s"; "d"; "f"; "g"; "h"; "j"; "k"; "l"]; 
  ["z"; "x"; "c"; "v"; "b"; "n"; "m"]]

let rec guess_list guesses = match guesses with 
| [] -> []
| h :: t -> h :: guess_list t

let make_keyboard guesses = assert false

let rec make_grid row column (guesses : ((string * int) list) list) : unit = 
  match guesses with 
  | [] -> empty_grid row column
  | h :: t -> colored_row column h; print_endline ""; make_grid (row - 1) column t

(** [make_game d l g] makes a game with [d] rows representing the difficulty,
[l] letter representing the number of letters, and with [g] guesses representing
the guesses so far.
Precondition : length of guesses is smaller then dif*)
let make_game dif letters guesses : unit =
  print_endline "Wordle 2.0 ( i ) ( l ) ( h ) ( r )";
  make_grid dif letters guesses

 
(** [dict] currently stores the five letter word dictionary, which is of type
string list*)
let dict = 5 |> load |> dict_lst

(** [correct_word] is the correct word *)
let correct_word = pick dict 

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
  in_dict dict str && (str != "i") && (str != "l") && (str != "h") && (str != "r") 


let print_history guess =
  let colored_guess = Processor.colorize_guess correct_word guess in
  print_word colored_guess

(** [end_screen ()] represents the state after the game ends. *)
let end_screen win () =
  if win then 
  (print_endline "\nCongratulations! You have guessed the correct word!";
  print_endline "Score: (To be implemented)";
  print_endline "") else (print_endline "\nYou didn't guess the word :(";
    print_endline ("The word was: " ^ correct_word))

(** represents the number of attempts the user has to guess the word. Will be
replaces with implementation based on the level of difficulty decided by
the user *)
let dif = 6

(** represents the number of letters of the word the user has to guess. Will be
replaces with implementation based on theselection by user *)
let letters = 5
let instructions = "\nInstructions:\nWelcome to Wordle 2.0, the goal of the game is to find the secret "^ string_of_int letters ^ " letter word.\n - To add a word into the game, type it into the terminal.\n - If the letter(s) in the word suggested is in the solution, but in the wrong position, it will come out as yellow.\n - If the letter(s) in the word suggested is in the solution, but in the correct position, it will come out as green.\n - If the letter(s) in the word suggested is not in the solution, it will come out as grey.\nThe end goal is to get the secret word in 6 tries or less.\n\nGood Luck!\n\n"
let leaderboard = "\nLeaderboard:\nTO BE IMPLEMENTED\n\n"
let hint = "\nHint:\nTO BE IMPLEMENTED\n\n"

let igCommand inp = match inp with
| "i" -> print_string(instructions)
| "l" -> print_string(leaderboard)
| "h" -> print_string(hint)
| _ -> print_string("")


(** [play ()] represents the in-game state. *)
let rec play (guesses : ((string*int) list)list) () =
  make_game dif letters guesses;
  let input = String.lowercase_ascii (read_line ()) in  
  if (in_check input) then (
  let output = input |> naive_processor in
  match output with
  | true -> make_game dif letters 
    (guesses @ [(colorize_guess correct_word input)]); end_screen true ()
  | false -> if (List.length guesses + 1) = dif then (
    make_game dif letters (guesses @ [(colorize_guess correct_word input)]);
    end_screen false ()) 
    else play (guesses @ [(colorize_guess correct_word input)]) () )
  else (if (input = "r") then (ANSITerminal.print_string [ ANSITerminal.Underlined ] "\n\nStarting New Game\n\n"; play [] ()) else if (input = "i" || input = "l" || input = "h") then igCommand(input) else  print_endline (input^" is not a valid word");
  play guesses ())

(** [start ()] represents the pre-game state. *)
    let start () =
  print_endline "Welcome to Wordle 2.0!";
  print_endline "Guess the five-letter word.";
  play [] ()

let () = start ()