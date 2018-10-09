let sum a b = a +. b

let () =
  print_float( sum 0.1 0.3);
  print_char '\n';
  print_float( sum (-0.1) 0.3);
  print_char '\n';
  print_float( sum 7.0 5.3);
  print_char '\n'