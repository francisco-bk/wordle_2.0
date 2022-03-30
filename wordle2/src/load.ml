(* The functions in this file loads the target dictionary from the data file, 
and assembles it into an ocaml format *)


let parse_dict (dict:string list) : string list= 
  List.map (fun x -> String.trim x) dict

let choose_word_length (length:int) (dict:string list): string list = 
  List.filter (fun x -> String.length x = length) dict

let load : in_channel = open_in "wordle2/data/words_alpha.txt"
  

let dictlst (file: in_channel) : string list = 
  let lines = ref [] in
  try
    while true; do
      lines := input_line file :: !lines
    done; !lines
  with End_of_file ->
    close_in file;
    List.rev !lines 








(* To use these in main, do the following:
load |> dictlst |> parse_dict |> choose_word_length [length]
*)



