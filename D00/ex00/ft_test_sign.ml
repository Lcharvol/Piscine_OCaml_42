let ft_test_sign nb =
    if nb < 0 then
        begin
            print_endline "negative"
        end
    else
        begin
            print_endline "positive"
        end

let () =
    ft_test_sign 42;
    ft_test_sign 0;
    ft_test_sign (-42);;