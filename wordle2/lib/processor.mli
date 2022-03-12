(** Processing of words in various ways. *)

val format : string -> string
(** [format s] cleans [s] by lowercasing all letters and removing leading and 
    trailing white spaces. *)

val is_valid : string -> bool
(** [is_valid s] is [true] if [s] is a word in the local dictionary and
    [false] otherwise. *)

val is_valid_naive : string -> bool
(** [is_valid s] is [true] if [s] only contains letters of the alphabet and
    [false] otherwise. *)

val match : string -> string -> bool
(** [match s1 s2] is [true] if [s1] represents the same word as [s2] and [false]
    otherwise.*)

val t
(** [t] is the abstraction of a word as an association list.
    Key: Letter present in the word.
    Value: List of indexes in which the character appears in the string.
    RI: The keys are ordered alphabetically. *)

val char_index : string -> t
(** [char_index s] formats [s] into [t]. *)

(* TODO: Add more functions that are able to identify whether two words match,
  and how each character differs.
  i.e. if a letter is present in a word, if the letter is in the right
  position. *)