(** Handles how the game loads a target dictionary from a data file.

    This module can load a dictionary representing a specific
    difficulty, and return the dictionary as an OCaml format. *)

val parse_dict : string list -> string list
(** [parse_dict dict] returns a cleaned list of all the words contained
    in' [dict]*)

val choose_word_length : int -> string list -> string list
(** [choose_word_length dict length] returns a list of all the words of
    length [length] contained in [dict]*)

val load : int -> in_channel
(** [load difficulty] loads the dictionary associated with [difficulty]
    from the data folder *)

val dict_lst : in_channel -> string list
(** [dict_lst file] converts [file] into a string list that contains all
    words*)

val str_to_lst : string -> string list
(** [str_to_lst str] converts [str] of format ["item1,item2,item3"] to a
    list of [\["item1"; "item2"; "item3"\]]. *)
