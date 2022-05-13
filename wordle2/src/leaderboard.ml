(* The functions in the file implement the leaderboard feature*)

let get_board (length:int): in_channel = 
  open_in (("wordle2/data/leaderboard" ^ (string_of_int length)^".txt"))


let str_to_lst (strings:string)  : string list = 
  let lst = strings |> String.split_on_char ';' 
    |> List.filter (fun x -> x <> "") 
    |> List.map (fun x -> String.trim x) in 
      List.map (fun x -> String.split_on_char ',' x) lst 
      |> List.flatten 
      |> List.filter (fun x -> x <> "")

let board_lst (file : in_channel) = 
    try  
      flush stdout;             
      let strings = input_line file in 
      (str_to_lst strings)
    with _ ->                    
      close_in_noerr file;          
      []

let format lst : string = 
  let new_player_lst = List.filter (fun x -> String.contains x ' ') lst in 
  let new_score_lst = List.filter ((fun x -> not (String.contains x ' '))) lst 
  |>List.map (fun x -> (print_endline x;int_of_string x)) in 
  let new_lst = List.combine new_player_lst new_score_lst in 
    List.fold_left (fun init x -> (fst x) ^ "" ^ "," ^ string_of_int (snd x) ^
     ";" ^ init) "" new_lst

let write (length:int )(msg:string ) = 
  (* Write message to file *)
  let oc = open_out 
    (("wordle2/data/leaderboard" ^ (string_of_int length)^".txt")) in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "%s" msg;
  (* write something *)
  close_out oc

let check_board (lst) : bool = 
  if List.length lst <> 0 then true else false 

let pick_first_five lst = 
  let new_player_lst = List.filter (fun x -> String.contains x ' ') lst in 
  let new_score_lst = List.filter ((fun x -> not (String.contains x ' '))) lst 
    |> List.map (fun x -> (int_of_string x)) in 
  let compare p1 p2 = 
    match p1,p2 with 
    |(_,s),(_,s') -> 
    if s > s' then 1
    else if s = s' then 0 
    else -1  in 
  let new_lst = List.combine new_player_lst new_score_lst in 
  let lst = List.sort compare new_lst |> List.rev  in 
    if List.length lst <= 5 then lst else [List.hd lst] @ 
      [List.nth lst 1] @ [List.nth lst 2] @ [List.nth lst 3] @ [List.nth lst 4]
