(* functions abstractions below *)

val parse_dict : string list -> string list
(** [parse_dict dict] returns a cleaned list of all the words contained
    in' [dict]*)

val choose_word_length : int -> string list -> string list
(** [choose_word_length dict length] returns a list of all the words of
    length [length] contained in [dict]*)

val load : int -> in_channel
(** [load] is the alpha dictionary from the data folder *)

val dict_lst : in_channel -> string list
(** [dict_lst file] converts [file] into a string list that contains all
    words*)
