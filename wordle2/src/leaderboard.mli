val get_board: int -> in_channel
(*[get_leader] loads the leaderboard*)

val check_board: 'a list -> bool 
(*[check_board] returns a bool depending on whether the leaderboard is empty*)

val write : int -> string -> unit 
(*[write] writes to the leaderboard txt files*)

val board_lst :in_channel -> string list 
(*[board_lst] returns a list that contains all player/score pairs*)

val pick_first_five : string list -> (string*int) list
(*[pick_first_five] returns a list that only contains the highest 5 player/score
 pairs *)


val format : string list -> string
(*[format] formats the output of [board_lst] into "name ,score;" so it can be 
written to the txt files.*)