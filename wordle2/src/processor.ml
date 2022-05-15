let format s = s |> String.uppercase_ascii |> String.trim
let are_equal answer guess = format answer = format guess

let in_dict (dict : string list) (word : string) : bool =
  let whats_left = dict |> List.filter (fun x -> x = word) in
  whats_left <> []

(** [s_to_list s] takes string [str] and returns a string list with all
    the characters in the word. *)
let rec s_to_list str =
  match str with
  | "" -> []
  | str ->
      let h = String.sub str 0 1 in
      h :: s_to_list (String.sub str 1 (String.length str - 1))

(** [find l e a] searches through list [l] for element [e] starting at
    index [acc] and returns the index of [e] Precondition: [e] is an
    element in [l] *)
let rec find lst element acc : int =
  let nth_ele = List.nth lst acc in
  if nth_ele = element then acc else find lst element (acc + 1)

(** [replace l i r] replaces the element at index [i] in list [l] with
    [r], if index [i] is not in range of the list [l] then [l] is
    returned *)
let replace lst index rep =
  List.mapi (fun i x -> if i = index then rep else x) lst

(** [remove_greens g gr r] takes guess [g] and removes all green
    indicies [gr] and replaces them with string [r]. *)
let rec remove_greens guess greens rep : string list =
  match greens with
  | [] -> guess
  | h :: t -> remove_greens (replace guess h rep) t rep

(** [green_list a g acc] returns list of indicies that indicate green
    coloring based on answer [a] and guess [g] starting at index [acc]. *)
let rec green_list answer guess acc : int list =
  match (answer, guess) with
  | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then acc :: green_list t1 t2 (acc + 1)
      else green_list t1 t2 (acc + 1)
  | _, _ -> []

(** [yellow_list a g gr acc] returns list of indicies that indicate
    yellow coloring based on answer [a], guess [g], and green indicies
    [gr] starting at index [acc]. *)
let rec yellow_list answer guess greens acc : int list =
  let guess = remove_greens guess greens "#" in
  match answer with
  | h :: t ->
      if
        (not (List.mem acc greens))
        && List.mem h guess
        && not (List.mem (find guess h 0) greens)
      then
        find guess h 0
        :: yellow_list t
             (replace guess (find guess h 0) "#")
             greens (acc + 1)
      else yellow_list t guess greens (acc + 1)
  | _ -> []

(** [combine gr y g a] combines list of green indicies [gr], list of
    yellow indicies [y], based on guess [g] starting at index [acc].
    Returns list of ints representing coloring of each index of guess
    [g], if green 2, if yellow 1, and if grey 0. *)
let rec combine greens yellows guess acc : int list =
  match guess with
  | [] -> []
  | _ :: t ->
      if List.mem acc greens then 2 :: combine greens yellows t (acc + 1)
      else if List.mem acc yellows then
        1 :: combine greens yellows t (acc + 1)
      else 0 :: combine greens yellows t (acc + 1)

let color_list answer guess : int list =
  let answer = s_to_list answer in
  let guess = s_to_list guess in
  let greens = green_list answer guess 0 in
  combine greens (yellow_list answer guess greens 0) guess 0

let colorize_guess answer guess =
  let colors = color_list answer guess in
  let guess_lst = s_to_list guess in
  List.combine guess_lst colors
