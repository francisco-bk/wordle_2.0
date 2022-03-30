(** Represents the history for the current wordle game 

    Game history includes: guess history, provided hints, keyboard colorings. *)

type hint
(** Representation of a hint. *)

type t
(** Representation of game history. *)

val init_hist : string -> t
(** [init_hist] is the history at the start of the game. *)

val add_guess : t -> string -> t
(** [add_guess w] is the history with guess [w] added.
    Precondition: [w] is a valid word according to the game configuration. *)

val get_hint : int -> t -> hint option * t
(** [get_hint id hist] is a [(hint option, hist)] pair for the hint and new
    history state. [hint option] is [Some hint] if there is a valid hint for
    [id] and [None] otherwise. [id] is 0 to represent a grey letter hint,
    1 to represent a yellow letter hint, and 2 to represent a green letter
    hint. *)

val hint_id : hint -> int
val hint_letter : hint -> string