let encode l =
    let rec count_letter l count acc = match l with
      | first_letter::second_letter::tail ->
        if(first_letter = second_letter)
          then
            count_letter (second_letter::tail) (count + 1) acc
        else
            count_letter (second_letter::tail) 0 (acc @ [(count + 1), first_letter])
      | letter::tail -> acc @ [(count + 1), letter]
      | []  -> acc
    in
    count_letter l 0 []

let rec print_tuples l = match l with
  | (count, letter)::tail -> print_int count; print_char letter; print_tuples tail
  | [] -> print_char '\n'

let main () =
  print_tuples (encode ['a'; 'a'; 'a'; 'b'; 'b'; 'b']);
  print_tuples (encode ['a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'a']);
  print_tuples (encode ['a'; 'b'; 'a'; 'b'; 'a'; 'b'; 'a']);
  print_tuples (encode [])

let () = main ()