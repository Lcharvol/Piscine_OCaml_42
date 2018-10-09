let get_random_joke jokes len =
  let rec get_random_joke_rec jokes id ac= match jokes with
    | joke::tail when ac = id -> print_endline joke
    | joke::tail -> get_random_joke_rec tail id (ac + 1) 
    | [] -> ()
  in
  Random.self_init ();
  get_random_joke_rec jokes (Random.int len) 0

let get_jokes in_channel =
  let rec get_next_line in_channels ac len =
    try
      let str = input_line in_channel in
      get_next_line in_channels (ac @ [str]) (len + 1)
    with
      | End_of_file -> get_random_joke ac len
      | _ -> get_random_joke ac len
  in
  get_next_line in_channel [] 0

let open_file file_name = 
  try
    let in_channel = open_in file_name
    in
    get_jokes in_channel
  with
    | Sys_error err -> print_endline ("Cant open " ^ file_name);
    | _ -> ()

let () =
  let argv = Array.to_list Sys.argv in
  if (List.length argv = 2)
  then 
    open_file (Sys.argv.(1))
