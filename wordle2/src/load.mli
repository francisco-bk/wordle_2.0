(* functions abstractions below *)

val load length : int -> string list 
(** [load length] loads one of the dictionaries from the data folder 
based on [length] and forms an [in_channel] type which is ready to be read 
Requires: [length] is of type int that represents the length of the word *)

val dict_lst file : int * in_channel -> string list 
(** [dict_lst file] transforms [file] into an ocaml stirng list whose 
elements are individual words *)

val str_to_lst str  : string  -> string list 
(** [str_to_lst strings] separates all the words contained in [strings] and 
assemble them into a list 
Requires: [strings] contains words with the same length , and enclosed 
by "" and separated by , *)