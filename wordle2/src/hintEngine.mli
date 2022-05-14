(** Represents the hint engine for the current wordle board. *)

type hint
(** Representation of a hint. *)

type t
(** Representation of game history. *)

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
(** [correct_word e] is the answer contained by [e]. *)

val unguessed_letters : t -> string list
(** [correct_word e] is the answer contained by [e]. *)

val grey_letters : t -> string list
(** [grey_letters e] is [e]'s known grey letters. *)

val yellow_letters : t -> string list
(** [correct_word e] is [e]'s known yellow letters. *)

val green_letters : t -> string list
(** [correct_word e] is [e]'s known green letters. *)