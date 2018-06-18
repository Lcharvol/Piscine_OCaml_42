let fibonacci n =
  let rec fibonacci_aux n a b =
    if n < 0 then -1
    else if n = 0 then b
    else if n = 1 then a
    else fibonacci_aux (n - 1) (a + b) a
  in
  fibonacci_aux n 1 0

let () =
  print_int (fibonacci (-42));
  print_char '\n';
  print_int (fibonacci 1);
  print_char '\n';
  print_int (fibonacci 3);
  print_char '\n';
  print_int (fibonacci 6);
  print_char '\n';