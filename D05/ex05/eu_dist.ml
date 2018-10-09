let rec get_sum_of_array l ac = match l with
  | nb::tail -> get_sum_of_array tail (ac +. nb)
  | [] -> ac

let eu_dist a b =
  let f = fun x y -> (x -. y) ** 2.0
  in
  let sum_array = Array.map2 f a b
  in
  let res = get_sum_of_array (Array.to_list sum_array) 0.0 in
  sqrt res

let () =
  let array1 = Array.make 1 1.0 in
  let array2 = Array.make 1 0.0 in
  print_float (eu_dist array1 array2);
  print_char '\n';
  let array1 = Array.make 3 2.0 in
  let array2 = Array.make 3 5.0 in
  print_float (eu_dist array1 array2);
  print_char '\n'