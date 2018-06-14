let rec ft_print_comb () =
  let rec start nb =
    if nb < 1000
    then
      begin
        print_int nb;
        print_string ", ";
        start (nb + 1)
      end
    else
      print_string "\n"
  in
  start 0

let () =
  ft_print_comb ()