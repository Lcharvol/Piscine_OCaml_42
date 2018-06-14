let ft_power nb p =
    int_of_float((float_of_int nb) ** (float_of_int p))

let () =
    print_int (ft_power 2 4);
    print_char('\n');
    print_int (ft_power 3 0);
    print_char('\n');
    print_int (ft_power 0 5);
    print_char('\n');