(** Representation of a guessed word. *)
type guessed_word = {
  word : string;
  colorization : int list (* Colorization of the word as given by Processor.color_list*)
}

type hint = {
  (* id: 0 and 1 is for grey and yellow letter hint, respectively. *)
  id : int;
  letter : string;
}

(** Representation of the colored keyboard. *)
type keyboard = {
  unguessed_letters : string list;
  grey_letters : string list;
  yellow_letters : string list;
  green_letters : string list;
}

type t = {
  (* guessed_words RI: the rightmost entry should be the earliest guessed word while the
     leftmost is the latest. They should be ordered in increasing age. *)  
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

(** [remove_from_unguessed kb l] is [kb] with letter [l] removed from the
    unguessed letters list. *)
let remove_from_unguessed keyboard letter =
  let unguessed = keyboard.unguessed_letters in
  let new_unguessed = List.filter (fun l -> l <> letter) unguessed in
  { keyboard with unguessed_letters = new_unguessed }


(** [update_keyboard_letter kb letter value] is [kb] with [letter] newly
    associated with the color represented by [val]. *)
let update_keyboard_letter keyboard letter value =
  let kb_unguessed = remove_from_unguessed keyboard letter in
  let greys = keyboard.grey_letters in
  let yellows = keyboard.yellow_letters in
  let greens = keyboard.green_letters in
  match value with
  | 0 ->
    if List.exists (fun l -> l = letter) greys then keyboard
    else let new_grey = letter :: greys in
      { kb_unguessed with grey_letters = new_grey }
  | 1 ->
    if List.exists (fun l -> l = letter) (greens @ yellows) then keyboard
    else let new_yellow = letter :: yellows in
      { kb_unguessed with yellow_letters = new_yellow }
  | 2 ->
    if List.exists (fun l -> l = letter) greens then keyboard
    else let new_yellow = List.filter (fun l -> l <> letter) yellows in
      {
        kb_unguessed with
        yellow_letters = new_yellow; green_letters = letter :: greens
      }
  | _ -> keyboard

(** [update_keyboard kb color_wd] is [kb] with new colored letters based on
    [color_wd]'s colorings.
    Precondition: [color_wd] is in the format of the output of
    Processor.colorize_guess. *)
let rec update_keyboard keyboard colorized_guess =
  match colorized_guess with
  | [] -> keyboard
  | (letter, value) :: t ->
    let keyboard = update_keyboard_letter keyboard letter value in
    update_keyboard keyboard t

let add_guess hist w = 
  let colors = Processor.color_list hist.answer w in
  let new_guessed_word = { word = w; colorization = colors } in
  let colorized_word = Processor.colorize_guess hist.answer w in
  {
    hist with
    guessed_words = new_guessed_word :: hist.guessed_words;
    keyboard = update_keyboard hist.keyboard colorized_word
  }

(** [add_hint_to_hist hist hint] is the history with the keyboard updated with
    [hint]'s information. *)
let add_hint_to_hist hist hint = { hist with hints = hint :: hist.hints }
(*TODO: Update keyboard with hint*)

(** [unguessed_yellows hist] is the list of yellow unguessed letters in state
    [hist]. *)
let unguessed_yellows hist = 
  let ungssd = hist.keyboard.unguessed_letters in
  List.filter (fun l -> String.contains hist.answer l.[0]) ungssd

(** [unguessed_greys hist] is the list of grey unguessed letters in state
    [hist]. *)
let unguessed_greys hist = 
  let ungssd = hist.keyboard.unguessed_letters in
  List.filter (fun l -> not (String.contains hist.answer l.[0])) ungssd

(** [get_rand_ele lst] is a random element from [lst]. *)
let get_rand_ele lst =
  let rand = Random.int (List.length lst) in
  List.nth lst rand

let get_hint id hist = match id with
| 0 -> 
  let hint_letter = hist |> unguessed_greys |> get_rand_ele in
  let hint = { id = 0; letter = hint_letter} in
  let new_hist = add_hint_to_hist hist hint in
  (Some hint, new_hist)
| 1 ->
  let hint_letter = hist |> unguessed_yellows |> get_rand_ele in
  let hint = { id = 1; letter = hint_letter} in
  let new_hist = add_hint_to_hist hist hint in
  (Some hint, new_hist)
| _ -> (None, hist)
