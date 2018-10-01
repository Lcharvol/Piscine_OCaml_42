let rec ft_print_comb () =
  let rec start a b c =
    print_int a;
    print_int b;
    print_int c;
    if a < 7
      then
        begin
          print_string ", ";
          if c < 9
            then start a b (c + 1)
          else if b < 8
            then start a (b + 1) (b + 2)
          else
            start (a + 1) (a + 2) (a + 3)
        end
      else
        print_string "\n"
  in
  start 0 1 2

let () =
  ft_print_comb ()