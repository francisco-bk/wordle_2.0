(** Represents the history for the current wordle game 

    Game history includes: guess history, provided hints, keyboard colorings. *)

type hint

type t

val init_hist : string -> t