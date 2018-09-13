let crossover l1 l2 = 
  let rec is_in_l2 l2 to_found = match l2 with
    | elem::tail ->
      if(elem = to_found)
        then
          true
      else
        is_in_l2 tail to_found
    | [] -> false
  in
  let rec map_l1 l1 l2 acc = match l1 with
    | elem::tail ->
      if((is_in_l2 l2 elem) = true)
        then
          map_l1 tail l2 (acc @ [elem])
      else
        map_l1 tail l2 acc
    | [] -> acc
  in
  map_l1 l1 l2 []

let rec print_list_of_char l = match l with
  | elem::tail -> print_char elem; print_list_of_char tail
  | [] -> print_char '\n'


let rec print_list_of_int l = match l with
  | elem::tail -> print_int elem; print_list_of_int tail
  | [] -> print_char '\n'

let main () =
  print_list_of_char (crossover ['1'; '2'; '3'; '4'; '5'] ['1'; '2']);
  print_list_of_char (crossover ['1'; '2'; '3'; '4'; '5'] ['1'; '2'; '3'; '4'; '5']);
  print_list_of_char (crossover [] ['1'; '2']);
  print_list_of_char (crossover ['1'; '2'] []);
  print_list_of_int (crossover [1; 2; 3; 4; 5] [1; 2])

let () = main ()