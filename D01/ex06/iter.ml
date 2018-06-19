let rec iter f x n =
  if n < 0 then -1
  else if n = 0 then x
  else f (iter f x (n - 1))

let () =
  print_int (iter (( * ) 2) 2 6);
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 4);
  print_char '\n'
