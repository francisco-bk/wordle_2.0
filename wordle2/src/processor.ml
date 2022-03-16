(** [word_ok dict s] is [true] if [s] only contains letters of the alphabet and
    in dictionary [dict]. Otherwise, is [false]. *)
let word_ok dict s = raise (Failure "Unimplemented")

(** [add_c_to_assoc i c lst] appends index [i] to the list of indices associated
    with key [c] in association list [lst]. If [c] is not a key in [lst], 
    a new key-value pair (c, [i]) will be appended to [lst].
    - The list of indices associated with each key will be sorted in ascending
    order.
    - The keys are not guaranteed to be sorted in the returned list.
    Requires: lst is an association list as with keys and values as defined in
    [char_index s] with no duplicate entries. *)
let add_c_to_assoc c i lst = match List.assoc_opt c lst with
| None -> (c, [i]) :: lst
| Some i_lst -> let clean_lst = List.remove_assoc c lst in
(c, i :: i_lst |> List.sort compare) :: clean_lst

(** [char_index s] formats [s] into an association list. 
    Keys: Letters in the string, sorted alphabetically.
    Values: List of indices in which the letter appears in the string, sorted
    in increasing order.
    Requires: [s] is a formatted word.
    Postcondition: There are no duplicate key entries. *)
let char_index s : (char * (int list)) list = 
  let rec char_index_aux s i lst = 
    if i < String.length s
    then (let c = s.[i] in char_index_aux s (i + 1) (add_c_to_assoc c i lst))
    else lst
  in List.sort compare (char_index_aux s 0 [])

let format s = s |> String.uppercase_ascii |> String.trim

let are_equal answer guess = answer = guess

let in_dict dict word = raise (Failure "Unimplemented")

let rec s_to_list str =
  match str with
    | "" -> []
    | str -> 
      let h = String.sub str 0 1 in
      h :: (s_to_list (String.sub str 1 ((String.length str)-1)))

let rec find lst element acc : int = 
  let nth_ele = List.nth lst acc in
  if nth_ele = element then acc 
  else find lst element (acc + 1)

let replace lst index rep = 
  List.mapi (fun i x -> if i = index then rep else x) lst

let rec check_green answer guess acc: int list = 
  match answer, guess with
  | h1 :: t1, h2 :: t2 -> 
    if (h1 = h2) then (acc :: check_green t1 t2 (acc+1)) 
    else check_green t1 t2 (acc+1)
  | _ , _ -> []

let rec check_yellow answer guess greens acc : int list = 
  match answer with 
  | h :: t -> if (List.mem h guess) && not (List.mem (find guess h 0) greens) 
    then (find guess h 0) :: 
      (check_yellow t (replace guess (find guess h 0) "#") greens (acc+1))
    else check_yellow t guess greens (acc+1)
  | _ -> []

let rec combine greens yellows guess acc: int list = 
  match guess with
  | [] -> []
  | _ :: t -> if (List.mem acc greens) 
    then 2 :: (combine greens yellows t (acc+1))
    else if (List.mem acc yellows) 
      then 1 :: (combine greens yellows t (acc+1))
      else 0 :: (combine greens yellows t (acc+1))


let check_green_yellow answer guess : int list = 
  let answer = s_to_list answer in
  let guess = s_to_list guess in
  let greens = check_green answer guess 0 in
  combine greens
   (check_yellow answer guess greens 0) 
    guess 0

let colorize_guess answer guess = 
  let colors = check_green_yellow answer guess in
  let guess_lst = s_to_list guess in
  List.combine guess_lst colors