(** Temporary word processor, to be replaced by functions from src files*)
let naive_processor = function 
| "camels" -> true
| _ -> false

let print_history () =
  print_endline "\nHistory:\n(Currently Unimplemented)"

let end_screen () =
  print_endline "\n\nCongratulations! You have guessed the correct word!";
  print_endline "Score: (To be implemented)"

(** [play ()] represents the in-game state. *)
let rec play () =
  print_string "\nEnter your guess: ";
  let input = read_line () |> naive_processor in
  match input with
  | true -> 
    end_screen ()
  | false ->
    print_endline "\n\nIncorrect!";
    print_history ();
    play ()

(** [start ()] represents the pre-game state. *)
    let start () =
  print_endline "Welcome to Wordle 2.0!";
  print_endline "Guess the five-letter word.";
  play()

let () = start ()
