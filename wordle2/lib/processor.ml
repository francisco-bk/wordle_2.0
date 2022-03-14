(** [word_ok s] is [true] if [s] only contains letters of the alphabet and in
  dictionary [dict]. Otherwise, is [false]. *)
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

let in_dict dict s = raise (Failure "Unimplemented")