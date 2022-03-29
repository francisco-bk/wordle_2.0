type guessed_word = {
  word : string;
  colorization : int list (* Colorization of the word as given by Processor.color_list*)
}

type hint = {
  (* Id 0 = grey letter hint, Id 1 = yellow letter hint, Id 2 = green letter hint *)
  id : int;
  letter : string;
  position : int; (* -1 if position not provided in hint. *)
}

type keyboard = {
  unguessed_letters : string list;
  grey_letters : string list;
  yellow_letters : string list;
  green_letters : string list;
}

type t = {
  (* guessed_words RI: the rightmost entry should be the earliest guessed word while the
     leftmost is the latest. They should be ordered in increasing age.*)  
  answer : string;
  guessed_words : guessed_word list;
  keyboard : keyboard;
  hints : hint list
}

let init_keyboard () = {
  unguessed_letters = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k";
    "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"];
  grey_letters = [];
  yellow_letters = [];
  green_letters = [];
}

let init_hist ans = {
  answer = ans;
  keyboard = init_keyboard ();
  guessed_words = [];
  hints = [];
}


(** returns keyboard with unguessed letter removed*)
let remove_from_unguessed keyboard letter =
  let new_unguessed = List.filter (fun l -> l <> letter) keyboard.unguessed_letters in
  { keyboard with unguessed_letters = new_unguessed }


(* update keyboard based on w's colorings *)
let rec update_keyboard keyboard colorized_word =
  match colorized_word with
  | [] -> keyboard
  | (letter, value) :: t ->
    match value with
    | 0 ->
      if List.exists (fun l -> l = letter) keyboard.grey_letters then
        update_keyboard keyboard t
      else
        let kb_unguessed = remove_from_unguessed keyboard letter in
        let new_grey = letter :: keyboard.grey_letters in
        let new_keyboard = { kb_unguessed with grey_letters = new_grey } in
        update_keyboard new_keyboard t
    | 1 ->
      if List.exists (fun l -> l = letter) (keyboard.green_letters @ keyboard.yellow_letters) then
        update_keyboard keyboard t
      else
        let kb_unguessed = remove_from_unguessed keyboard letter in
        let new_yellow = letter :: keyboard.yellow_letters in
        let new_keyboard = { kb_unguessed with yellow_letters = new_yellow } in
        update_keyboard new_keyboard t
    | 2 ->
      if List.exists (fun l -> l = letter) keyboard.green_letters then
        update_keyboard keyboard t
      else
        let kb_unguessed = remove_from_unguessed keyboard letter in
        let new_yellow = List.filter (fun l -> l <> letter) keyboard.yellow_letters in
        let new_green = letter :: keyboard.green_letters in
        let new_keyboard = { kb_unguessed with yellow_letters = new_yellow; green_letters = new_green } in
        update_keyboard new_keyboard t
    | _ -> raise (Failure "Invalid value")


(** [add_word w] adds word to the history. Precondition: [w] is a valid word according to the game configuration. *)
let add_word hist w = 
  let colors = Processor.color_list hist.answer w in
  let new_guessed_word = { word = w; colorization = colors } in
  let new_guessed_words = new_guessed_word :: hist.guessed_words in
  let colorized_word = Processor.colorize_guess hist.answer w in
  let new_keyboard = update_keyboard hist.keyboard colorized_word in
  { hist with guessed_words = new_guessed_words; keyboard = new_keyboard }
  
  