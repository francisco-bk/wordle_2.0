(** Processing of words in various ways. *)

val format : string -> string
(** [format s] cleans [s] by capitalizing all letters and removing leading and 
    trailing white spaces. *)

val are_equal : string -> string -> bool
(** [are_equal answer guess] is [true] if [answer] represents the same word as
    [guess] and [false] otherwise. They are the same word if both contain the
    same sequence of letters in the same order.
    Requires: [s1] and [s2] contain only lowercase letters with no whitespace.*)

val in_dict : string -> int list -> bool
(** [in_dict s] is [true] if [s] is a word in the local dictionary and
    [false] otherwise. *)