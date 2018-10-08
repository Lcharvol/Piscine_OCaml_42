
let my_sleep time = Unix.sleep time

let main time =
    let get_time = 
      try (int_of_string time)
      with _ -> 0
    in
    my_sleep get_time

let () =
  let argv = Array.to_list Sys.argv in
  if (List.length argv = 2)
  then main (Sys.argv.(1))

(* ocamlopt unix.cmxa micronap.ml*)