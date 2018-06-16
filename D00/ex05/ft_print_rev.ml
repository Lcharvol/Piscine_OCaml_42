let ft_print_rev str =
  let rec ft_print_letter n =
    if n >= 0
      then
        begin
          print_char (String.get str n);
          ft_print_letter (n - 1)
        end
      else
        print_char '\n'
  in
  ft_print_letter ((String.length str) - 1)
let () =
  ft_print_rev "Hello world !";
  ft_print_rev "";