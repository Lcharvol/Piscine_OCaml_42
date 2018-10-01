let ft_sum f il iu =
  let rec ft_sum_aux ac f il =
    if il > iu
    then ac
    else ft_sum_aux (ac +. (f il)) f (il + 1)
  in
  ft_sum_aux 0. f il

let () = 
    print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
    print_char '\n'