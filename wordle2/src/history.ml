(** Representation of a guessed word. *)
type guessed_word = {
  word : string;
  colorization : int list (* Colorization of the word as given by Processor.color_list*)
}

type hint = {
  (* id: 0, 1, 2 is for grey, yellow, and green letter hint, respectively. *)
  id : int;
  letter : string;
  position : int; (* -1 if position not provided in hint. *)
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
  let new_unguessed = List.filter (fun l -> l <> letter) keyboard.unguessed_letters in
  { keyboard with unguessed_letters = new_unguessed }


(** [update_keyboard kb color_wd] is [kb] with new colored letters based on
    [color_wd]'s colorings.
    Precondition: [color_wd] is in the format of the output of
    Processor.colorize_guess. *)
let rec update_keyboard keyboard colorized_guess =
  match colorized_guess with
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


let add_guess hist w = 
  let colors = Processor.color_list hist.answer w in
  let new_guessed_word = { word = w; colorization = colors } in
  let new_guessed_words = new_guessed_word :: hist.guessed_words in
  let colorized_word = Processor.colorize_guess hist.answer w in
  let new_keyboard = update_keyboard hist.keyboard colorized_word in
  { hist with guessed_words = new_guessed_words; keyboard = new_keyboard }
  
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

(* id is hint id (see History.hint) *)
let get_hint id hist = match id with
| 0 -> 
  let ungssd = unguessed_greys hist in
  let hint_letter = get_rand_ele ungssd in
  let hint = { id = 0; letter = hint_letter; position = -1} in
  let new_hist = add_hint_to_hist hist hint in
  (Some hint, new_hist)
| 1 ->
  let ungssd = unguessed_yellows hist in
  let hint_letter = get_rand_ele ungssd in
  let hint = { id = 1; letter = hint_letter; position = -1} in
  let new_hist = add_hint_to_hist hist hint in
  (Some hint, new_hist)
| 2 ->
  let ungssd = unguessed_yellows hist in
  let hint_letter = get_rand_ele ungssd in
  let pos = String.index hist.answer hint_letter.[0] in
  let hint = { id = 2; letter = hint_letter; position = pos} in
  let new_hist = add_hint_to_hist hist hint in
  (Some hint, new_hist)
| _ -> (None, hist)
