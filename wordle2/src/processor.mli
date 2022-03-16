(** Processing of words in various ways. *)

val char_index : string -> (char * int list) list

val format : string -> string
(** [format s] cleans [s] by capitalizing all letters and removing leading and 
    trailing white spaces. *)

val are_equal : string -> string -> bool
(** [are_equal answer guess] is [true] if [answer] represents the same word as
    [guess] and [false] otherwise. They are the same word if both contain the
    same sequence of letters in the same order.
    Requires: [s1] and [s2] contain only lowercase letters with no whitespace.*)

(* val in_dict : string -> int list -> bool *)
(** [in_dict s] is [true] if [s] is a word in the local dictionary and
    [false] otherwise. *)

val green_list : string list -> string list -> int -> int list

val yellow_list : string list -> string list -> int list -> int -> int list

val combine : int list -> int list -> string list -> int -> int list

val color_list : string -> string -> int list

val colorize_guess : string -> string -> (string * int) list