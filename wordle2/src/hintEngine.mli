(** Represents the hint engine for the current wordle board.

    This module is the backend for hint generation for the current
    Wordle board. Whenever a word is guessed, the engine internally
    updates its history and bases new hints off of the current history. *)

type hint
(** Representation of a hint. Contains the hint type (either a letter in
    the answer or a letter not in the answer), as well as the letter
    itself. *)

type t
(** Representation of a hint engine instance for a game. Contains the
    guess history, including unguessed letters as well as the grey,
    yellow, and green letters from the user's guesses. *)

val init_engine : string -> t
(** [init_engine] initializes the hint engine to represent the start of
    the game. *)

val add_guess : t -> string -> t
(** [add_guess w] is the history with guess [w] added. Precondition: [w]
    is a valid word according to the game configuration. *)

val get_hint : int -> t -> hint option * t
(** [get_hint id hist] is a [(hint option, hist)] pair for the hint and
    new history state. [hint option] is [Some hint] if there is a valid
    hint for [id] and [None] otherwise. [id] is 0 to represent a grey
    letter hint, 1 to represent a yellow letter hint, and 2 to represent
    a green letter hint. *)

val hint_id : hint -> int
(** [hint_id h] is the id of hint [h]. [0] represents a hint for a grey
    letter, and [1] represents a hint for a yellow letter.*)

val hint_letter : hint -> string
(** [hint_letter h] is the letter given by hint [h]. *)

val get_hint_tup : t -> (string * int) list
(** [get_hint_tup eng] is the color representations of all hints stored
    in [eng]. *)

val correct_word : t -> string
(** [correct_word eng] is the answer contained by [eng]. *)

val unguessed_letters : t -> string list
(** [unguessed_letters eng] is the answer contained by [eng]. *)

val grey_letters : t -> string list
(** [grey_letters eng] is [eng]'s known grey letters. *)

val yellow_letters : t -> string list
(** [yellow_letters eng] is [eng]'s known yellow letters. *)

val green_letters : t -> string list
(** [green_letters eng] is [eng]'s known green letters. *)