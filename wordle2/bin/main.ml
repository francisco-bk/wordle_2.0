open Game
open Processor
open Randpick
open Load

(** [dict] initializes itself to an empty list as a mutable field *)
let dict : string list ref = ref []

(** [dict_f length] returns a string list of all words based on the user-input
    [length] *)
let dict_f (l:int): string list = l |> load |> dict_lst

(** [correct_word] initializes itself to an empty string as a mutable field*)
let correct_word : string ref = ref ""

(** [length] represents the length of the word. It initializes itself to 0 *)
let length : int ref = ref 0

(** [penalties] represents the penalties in a game. I initializes itself to 0 *)
let penalties : int ref = ref 0

(** [difficulty] represents the diffuculty of the game. It initializes itself
    to 0 *)
let difficulty : int ref = ref 0

(** [score a l p] represents the score of the user upon completion based on
    attemps [a] length [l] and penalties [p].
    Precondition: p cannot be more than the length.*)
let score a l p = string_of_int (max 0 ((10 - a) * l - p))

(** [hint_engine] represents the hint engine of the game. It initializes itself
    to a new engine. *)
let hint_engine : HintEngine.t ref = ref (HintEngine.init_engine "")

(** [correctword length] picks the correct word bsaed on [length]*)
let correctword (length:int) :string =  (pick (dict_f length))

let rec empty_row column : string =
  if column = 0 then "|" else "|   " ^ empty_row (column - 1)

let rec empty_grid row column : unit = 
  let e_line = empty_row column in 
    if row = 0 then () else ((print_endline (String.make ((40 - 4*column)/2) ' ' ^ e_line)); 
    empty_grid (row - 1) column)
    
let print_color_letter c_tuple = match c_tuple with
  | (c, 0) -> print_string c
  | (c, 1) -> ANSITerminal.print_string [ ANSITerminal.yellow ] c;
  | (c, 2) -> ANSITerminal.print_string [ ANSITerminal.green ] c;
  | (c, 3) -> ANSITerminal.print_string [ ANSITerminal.red ] c;
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
  | h :: t -> h @ guess_list t

let rec make_colored_row row (guesses : (string * int) list) : unit = 
  match row with
  | [] ->  print_string "|"
  | h :: t -> print_string "| "; (if (List.mem (h, 2) guesses) then 
    print_color_letter (h, 2) else
      if (List.mem (h, 1) guesses) then print_color_letter (h, 1) else
        if (List.mem (h, 0) guesses) then print_color_letter (h, 3) 
        else print_string h); print_string " "; make_colored_row t guesses

let make_row row guesses = 
  let row = List.nth keyboard row in
    make_colored_row row guesses

let add_hints guesses =
  let hints = HintEngine.get_hint_tup !hint_engine in 
    hints @ guesses

let make_keyboard guesses = 
  let guesses = guess_list guesses in 
  let guesses = add_hints guesses in
    print_endline ""; (make_row 0 guesses);
    print_endline ""; print_string "  "; (make_row 1 guesses);
    print_endline ""; print_string "      "; (make_row 2 guesses);
    print_endline ""

let rec make_grid row column (guesses : ((string * int) list) list) : unit =  
  match guesses with 
  | [] -> empty_grid row column
  | h :: t -> print_string (String.make ((40 - 4*column)/2) ' '); 
    colored_row column h; print_endline "";
    make_grid (row - 1) column t

(** [make_game d l g] makes a game with [d] rows representing the difficulty,
    [l] letter representing the number of letters, and with [g] guesses representing
    the guesses so far.
    Precondition : length of guesses is smaller then dif*)
let make_game dif letters guesses : unit =
  print_endline ("    Wordle 2.0 ( i ) ( l ) ( h ) ( r )");
  make_grid dif letters guesses;
  make_keyboard guesses

(** [naive_processor a] Temporary word processor *)
let naive_processor a = 
  a = !correct_word

let rec print_word colored_word = 
  match colored_word with
  | [] -> ()
  | h :: t -> print_color_letter h;
  print_word t

(** [in_check str] check if the str is a valid word by comparing it to dict*)
let in_check str : bool=
  in_dict !dict str && 
    (str != "i") && (str != "l") && (str != "h") && (str != "r")

let print_HintEngine guess =
  let colored_guess = Processor.colorize_guess !correct_word guess in
  print_word colored_guess

(** [end_screen ()] represents the state after the game ends. *)
let end_screen guesses win () =
  let final_score = score (List.length guesses + 1) !length !penalties in
  if win then 
  (print_endline "\nCongratulations! You have guessed the correct word!";
  print_endline ("Score: " ^ final_score);
  print_endline "") else (print_endline "\nYou didn't guess the word :(";
    print_endline ("The word was: " ^ !correct_word))
let instructions = "\nInstructions:\nWelcome to Wordle 2.0, the goal of the \
  game is to find the secret "^ string_of_int !length ^ " letter word.\n - To \
  add a word into the game, type it into the terminal.\n - If the letter(s) \
  in the word suggested is in the solution, but in the wrong position, it will \
  come out as yellow.\n - If the letter(s) in the word suggested is in the \
  solution, but in the correct position, it will come out as green.\n - If \
  the letter(s) in the word suggested is not in the solution, it will come \
  out as grey.\nThe end goal is to get the secret word in 6 tries or less.\
  \n\nGood Luck!\n\n"
let leaderboard = "\nLeaderboard:\nTO BE IMPLEMENTED\n\n"
let hint = "Get a (yellow) letter hint, (grey) letter hint, or (cancel)."

let print_hint hint =
  let id = HintEngine.hint_id hint in
  let letter = HintEngine.hint_letter hint in
  if id = 0 then print_endline ("This letter is not in the word: " ^ letter)
  else print_endline ("This letter is in the word: " ^ letter)

let rec prompt_hint () =
  print_endline(hint);
  match read_line () with
  | "grey" ->
    (match HintEngine.get_hint 0 !hint_engine with
    | (Some hint, new_hist) -> print_hint hint; hint_engine := new_hist;
      penalties := !penalties + 1;
    | (None, _) -> print_endline "There are no more grey letters!";
      prompt_hint ())
  | "yellow" -> 
    (match HintEngine.get_hint 1 !hint_engine with
    | (Some hint, new_hist) -> print_hint hint; hint_engine := new_hist;
      penalties := !penalties + 1;
    | (None, _) -> print_endline "There are no more yellow letters!";
      prompt_hint ())
  | "cancel" -> ()
  | _ -> print_endline "Your input is invalid."; prompt_hint ()

let igCommand inp = match inp with
  | "i" -> print_string(instructions)
  | "l" -> print_string(leaderboard)
  | "h" -> prompt_hint ()
  | _ -> print_string("")

(** [choose_length ()] prompts the player to choose the length of the word
    they want.*)
let rec choose_length () = 
  print_endline "Please choose a number between 2 and 10, inclusive, to be the \
  length: ";
  print_string "> ";
  try
    let x = int_of_string (read_line ()) in
    if (1 < x && x < 11) then (correct_word := correctword x; length := x; 
      dict := dict_f x; hint_engine := HintEngine.init_engine !correct_word) 
    else (print_endline "This is not a valid number!"; choose_length ())
  with Failure _ -> 
    (print_endline "This is not a valid number!"; choose_length ())

(** [choose_difficulty ()] prompts the player to choose a difficulty.*)
let rec choose_difficulty () =
print_endline "Please choose a difficulty between 1 and 10, 
which represent the number of attempts.";
  print_string "> ";
  try 
    let x = int_of_string (read_line ()) in
      if (0 < x) && (x < 11) then (difficulty := x) 
      else (print_endline "This is not a valid difficulty!"; 
        choose_difficulty ())
  with Failure _ ->
    (print_endline "This is not a valid number!"; choose_length ())



(** [play ()] represents the in-game state. *)
let rec play (guesses : ((string*int) list)list) dif letters =
  make_game dif letters guesses;
  print_string "> ";
  let input = String.lowercase_ascii (read_line ()) in
  if (in_check input) then (
  let output = input |> naive_processor in
  match output with
  | true -> make_game dif letters 
    (guesses @ [(colorize_guess !correct_word input)]); end_screen guesses true ()
  | false ->
    hint_engine := HintEngine.add_guess !hint_engine input;
    if (List.length guesses + 1) = dif
    then (
      make_game dif letters (guesses @ [(colorize_guess !correct_word input)]);
      end_screen guesses false ()) 
    else
      play (guesses @ [(colorize_guess !correct_word input)]) dif letters )
  else command_choice dif letters guesses input

(** [command_choice dif letters guesses input] executes a command in play that
    is not a valid word*)
and command_choice dif letters guesses input  = 
  if (input = "r") then (
    ANSITerminal.print_string [ ANSITerminal.Underlined ] 
    "\n\nStarting New Game\n\n";
    correct_word := correctword letters;
    penalties := 0;
    hint_engine := HintEngine.init_engine !correct_word;
    play [] dif letters) 
  else if (input = "i"  || input = "l" || input = "h") 
  then (
    igCommand input;
    play guesses dif letters)
  else (
    print_endline (input ^ " is not a valid word");
    play guesses dif letters)

(** [start ()] represents the pre-game state. *)
let start () =
  print_endline instructions;
  print_endline "Press ANY key and ENTER to start!";
  print_string "> ";
  let _ = String.lowercase_ascii (read_line ()) in
  choose_length ();
  choose_difficulty ();
  play [] !difficulty !length

let () = start ()