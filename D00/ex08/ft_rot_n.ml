let ft_rot_n n str =
  let isAlpha c = c >= 'a' && c <= 'z'
  in
  let isCapitalizedAlpha c = c >= 'A' && c <= 'Z'
  in
  let rotateChar c =
    if isAlpha c
      then
        if (int_of_char c + n) > 122
          then char_of_int (97 + (int_of_char c + n - 123))
          else char_of_int (int_of_char c + n)
      else 
        if isCapitalizedAlpha c
          then
            if (int_of_char c + n) > 90
              then char_of_int (65 + (int_of_char c + n - 91))
              else char_of_int (int_of_char c + n)
          else c
  in
  String.map rotateChar str

let () =
  print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_endline (ft_rot_n 42 "0123456789");
  print_endline (ft_rot_n 2 "OI2EAS67B9");
  print_endline (ft_rot_n 0 "Damned !");
  print_endline (ft_rot_n 42 "");
  print_endline (ft_rot_n 1 "NBzlk qnbjr !");