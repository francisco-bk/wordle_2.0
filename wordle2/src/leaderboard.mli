(** Handles the leaderboard feature of the game.

    This module is the interface between the leaderboard text files in
    the data folder and OCaml. It is able to read the text files to
    display in the terminal, and also write to the text files based on a
    terminal player's score. *)

val get_board : int -> in_channel
(** [get_board difficulty] loads the leaderboard associated with
    [difficulty]. *)

val check_board : 'a list -> bool
(** [check_board board] returns [true] if the [board] is nonempty and
    [false] otherwise. *)

val write : int -> string -> unit
(** [write difficulty msg] writes [msg] to the end of the leaderboard
    data file associated with [difficulty]. *)

val board_lst : in_channel -> string list
(** [board_lst file] returns a list that contains all player/score pairs
    contained in [file]. *)

val pick_first_five : string list -> (string * int) list
(** [pick_first_five board] returns a list that contains the highest
    five player/score pairs in [board] *)

val format : string list -> string
(** [format board_lst] formats the output of [board_lst] into a
    repeating string with syntax "name,score;"*)
