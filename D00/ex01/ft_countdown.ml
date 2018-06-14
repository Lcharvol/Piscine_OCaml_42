let rec ft_countdown nb =
    if nb <= 0 then
        begin
            print_endline "0"
        end
    else
        begin
            print_endline (string_of_int (nb));
            ft_countdown (nb - 1)
        end

let () =
    ft_countdown 3;
    print_char '\n';
    ft_countdown 0;
    print_char '\n';
    ft_countdown (-1);
    print_char '\n';