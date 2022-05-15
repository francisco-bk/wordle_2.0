(** Processing of words for display in the terminal.

    This module handles the parsing of guesses inputted by users and
    colorizes guessed words based on an answer. *)

val format : string -> string
(** [format s] cleans [s] by capitalizing all letters and removing
    leading and trailing white spaces. *)

val are_equal : string -> string -> bool
(** [are_equal answer guess] is [true] if [answer] represents the same
    word as [guess] and [false] otherwise. They are the same word if
    both contain the same sequence of letters in the same order,
    invariant to white space and capitalization. *)

val in_dict : string list -> string -> bool
(** [in_dict s] is [true] if [s] is a word in the loaded dictionary and
    [false] otherwise. *)

val color_list : string -> string -> int list
(** [color_list a g] returns an int list representing how to color guess
    [g] based on answer [a]. Returns a list of 0's, 1's and 2's
    representing gray, yellow, and green for the coloring. Precondition
    : both the answer and the guess have the same number of letters. *)

val colorize_guess : string -> string -> (string * int) list
(** [colorize_guess answer guess] returns a representation of how each
    letter in [guess] should be colored. The pairs are in order of how
    the letters of [guess] appear, with each pair representing a single
    letter. Its associated value is 0 if the letter should be colored
    grey, 1 if the letter should be yellow, and 2 if the letter is
    green. *)
