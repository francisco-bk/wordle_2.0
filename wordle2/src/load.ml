(* The functions in this file loads the target dictionary from the data file, 
and assembles it into an ocaml format *)

let parse_dict (dict:string list) : string list= 
  List.map (fun x -> String.trim x) dict

let choose_word_length (length:int) (dict:string list): string list = 
  List.filter (fun x -> String.length x = length) dict

  
let load (length:int) : in_channel = 
  open_in ("wordle2/data/" ^ (string_of_int length) ^ ".txt")
  
let str_to_lst (strings:string)  : string list = 
  strings |> String.split_on_char ',' |> List.map (fun x -> String.trim x) 
  
let dict_lst (file : in_channel) = 
  try  
    flush stdout;             
    let strings = input_line file in 
      (str_to_lst strings)
  with e ->                    
    close_in_noerr file;          
    raise e

(* To use these in main, do the following:
[length] |> load |> dict_lst 
*)