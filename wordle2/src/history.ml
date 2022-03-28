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
  unguessed_letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
    "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"];
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

(* * [add_word w] adds word to the history. Precondition: [w] is a valid word according to the game configuration.
let add_word w = 
  Processor.color_list *)