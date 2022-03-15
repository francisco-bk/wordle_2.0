let rec play () =
  print_string "Enter your guess: ";
  let input = read_line () in
  match input with
  | "" -> exit 0
  | _ -> play ()

let start () =
  print_endline "Welcome to Wordle 2.0!";
  print_endline "Guess the five-letter word.";
  play()

let () = start ()
