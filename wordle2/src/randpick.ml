

let pick (dict:string list) : string = 
  Random.self_init ();
  let length = List.length dict in 
    let random = Random.int length in
      List.nth dict random 

