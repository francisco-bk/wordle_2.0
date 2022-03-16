(* The functions in this file loads the target dictionary from the data file, 
and assembles it into an ocaml format *)



let load (length:int): in_channel = 
  if length = 5 then open_in "5 letter words"
  else open_in "5 letter words"
  
  (* else if length = 4 then 
  else if length = 6 then open_in ""
  else if length = 7 then open_in "" 
  else *)

  (* Different files for different lengths of words*)

  let str_to_lst (str:string)  : string list = 
let strings = String.map (fun x -> if x = '"' then ' ' else x) str in
strings |> String.split_on_char ',' |> List.map (fun x -> String.trim x) 

  let dict_lst (file : in_channel) = 
    try  
            flush stdout;             (* write on the underlying device now *)
            
            let strings = input_line file in (* read line, discard \n *)
            (str_to_lst strings)
        with e ->                     (* some unexpected exception occurs *)
          close_in_noerr file;          (* emergency closing *)
          raise e

let str_to_lst (str:string)  : string list = 
let strings = String.map (fun x -> if x = '"' then ' ' else x) str in
strings |> String.split_on_char ',' |> List.map (fun x -> String.trim x) 


let str_of_char (cha:char) : string =
 String.make 1 cha






(* To use these in main, do the following:
[length] |> load |> dict_lst 
*)