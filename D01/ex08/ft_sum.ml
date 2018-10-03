let ft_sum f il iu =
  if il > iu then nan else
  let rec ft_sum_aux ac il =
    if il > iu then ac
    else ft_sum_aux (ac +. (f il)) (il + 1)
  in
  ft_sum_aux 0. il

let () = 
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
    print_char '\n';
    print_float (ft_sum (fun i -> float_of_int (i * i)) 4 100);
    print_char '\n';
    print_float (ft_sum (fun i -> float_of_int (i * i)) 4 1);
    print_char '\n'