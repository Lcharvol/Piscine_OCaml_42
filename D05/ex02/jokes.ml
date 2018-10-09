let jokes = [
  "Is your daddy the owner of Wendy's because you make me want to eat great even late!"
  ;
  "Are you a lightswitch? Why? Cause you really turn me on."
  ;
  "Why did God invent Jameson whiskey? So the Irish would never rule the world!"
  ;
  "How does a man show he's planning for the future?  He buys two cases of Miller Lite instead of one."
  ;
  "What is the difference between a sofa and a man watching Monday Night Football?  The sofa doesn't keep asking for Bud Light!"
  ]

let get_random_joke jokes =
  let rec get_random_joke_rec jokes id ac= match jokes with
    | joke::tail when ac = id -> print_endline joke
    | joke::tail -> get_random_joke_rec tail id (ac + 1) 
    | [] -> ()
  in
  Random.self_init ();
  get_random_joke_rec jokes (Random.int 5) 0

let () =
  get_random_joke jokes