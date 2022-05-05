(** Representation of a guessed word. [word] is the guessed word, while
    [colorization] represents the color of each letter in [word].
    RI: The colorization of the word is a list of 0's, 1's, or 2's of the same
        length as [word]. *)
type guessed_word = {
  word : string;
  colorization : int list
}


(** Representation of a hint. The [id] is [0] if it is a hint for a grey word,
    and [1] if it is a hint for a yellow word. [letter] is the letter given in
    the hint. *)
type hint = {
  id : int;
  letter : string;
}

(** Representation of the colored keyboard for letters a to z. *)
type keyboard = {
  unguessed_letters : string list;
  grey_letters : string list;
  yellow_letters : string list;
  green_letters : string list;
}

(** Representation of the hint engine. [answer] is the correct word for the
    current wordle game. [guessed_words] are the words that the player
    has guessed so far. [keyboard] keeps a record of what letters the player
    has used. [hints] records which hints are given to the player. *)
type t = {
  answer : string;
  guessed_words : guessed_word list;
  keyboard : keyboard;
  hints : hint list
}

(** [init_keyboard ()] initializes the keyboard with all unguessed letters. *)
let init_keyboard () = {
  unguessed_letters = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k";
    "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"];
  grey_letters = [];
  yellow_letters = [];
  green_letters = [];
}

let init_engine ans = {
  answer = ans;
  keyboard = init_keyboard ();
  guessed_words = [];
  hints = [];
}

let hint_id hint = hint.id
let hint_letter hint = hint.letter

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

let add_guess engine w = 
  let colors = Processor.color_list engine.answer w in
  let new_guessed_word = { word = w; colorization = colors } in
  let colorized_word = Processor.colorize_guess engine.answer w in
  {
    engine with
    guessed_words = new_guessed_word :: engine.guessed_words;
    keyboard = update_keyboard engine.keyboard colorized_word
  }

(** [update_poss_hints engine hint] is the hint engine with the keyboard updated
    with [hint]'s information. *)
let update_poss_hints engine hint = 
  let pair = (hint.letter, hint.id) in
  let updated_keyboard = update_keyboard engine.keyboard [ pair ] in
  { engine with hints = hint :: engine.hints; keyboard = updated_keyboard }

(** [unguessed_yellows engine] is the list of yellow unguessed letters in state
    [engine]. *)
let unguessed_yellows engine = 
  let ungssd = engine.keyboard.unguessed_letters in
  List.filter (fun l -> String.contains engine.answer l.[0]) ungssd

(** [unguessed_greys engine] is the list of grey unguessed letters in state
    [engine]. *)
let unguessed_greys engine = 
  let ungssd = engine.keyboard.unguessed_letters in
  List.filter (fun l -> not (String.contains engine.answer l.[0])) ungssd

(** [get_rand_ele lst] is a random element from [lst]. *)
let get_rand_ele lst =
  let rand = Random.int (List.length lst) in
  List.nth lst rand

let get_hint id engine = try match id with
  | 0 -> 
    let hint_letter = engine |> unguessed_greys |> get_rand_ele in
    let hint = { id = 0; letter = hint_letter} in
    let new_engine = update_poss_hints engine hint in
    (Some hint, new_engine)
  | 1 ->
    let hint_letter = engine |> unguessed_yellows |> get_rand_ele in
    let hint = { id = 1; letter = hint_letter} in
    let new_engine = update_poss_hints engine hint in
    (Some hint, new_engine)
  | _ -> (None, engine)
with Invalid_argument _ -> (None, engine)

let rec get_hints hints = 
  match hints with
  | [] -> []
  | h :: t -> (h.letter, h.id) :: get_hints t

let get_hint_tup engine = 
  let hints = engine.hints in
    get_hints hints

let correct_word e = e.answer
let unguessed_letters e = e.keyboard.unguessed_letters
let grey_letters e = e.keyboard.grey_letters
let yellow_letters e = e.keyboard.yellow_letters
let green_letters e = e.keyboard.green_letters