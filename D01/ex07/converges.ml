let rec converges f x n =
  if n < 0 then false
  else if n = 0 then f (x) = x
  else converges f (f x) (n - 1)

let () =
  if converges (( * ) 2) 2 5 = true
  then print_endline "True"
  else print_endline "False"
  ;
  if converges (fun x -> x / 2) 2 3 = true
  then print_endline "True"
  else print_endline "False"
  ;
  if converges (fun x -> x / 2) 2 2 = true
  then print_endline "True"
  else print_endline "False"
  ;