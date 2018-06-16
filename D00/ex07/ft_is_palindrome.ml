let ft_is_palindrome str =
  let is_equals a b = a = b
  in
  let rec check_letters i1 i2 =
    if i1 >= i2
      then true
      else
        if is_equals (String.get str i1) (String.get str i2) = false
          then false
          else check_letters (i1 + 1) (i2 - 1)
  in
  check_letters 0 ((String.length str) - 1)

let () =
  if ft_is_palindrome "radar" = true
    then print_endline "True"
    else print_endline "False"
  ;
  if ft_is_palindrome "madam" = true
    then print_endline "True"
    else print_endline "False"
  ;
  if ft_is_palindrome "car" = true
    then print_endline "True"
    else print_endline "False"
  ;
  if ft_is_palindrome "" = true
    then print_endline "True"
    else print_endline "False"