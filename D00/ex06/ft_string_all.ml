let ft_string_all f str =
  let rec test_char i =
    if i >= (String.length str) - 1
      then true
      else
        if f (String.get str i) = false
          then false
          else test_char (i + 1)
  in
  test_char 0

  
  let () = 
    let is_digit c = c >= '0' && c <= '9'
    in
    if ft_string_all is_digit "0123456789" = true
      then
        print_string "True\n"
      else
        print_string "False\n"
    ;
    if ft_string_all is_digit "01234e5678r9" = true
        then
          print_string "True\n"
        else
          print_string "False\n"
