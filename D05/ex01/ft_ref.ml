type 'a ft_ref = { mutable content : 'a }

let return x =
  let (new_ref: 'a ft_ref) = {content = x}
  in
  new_ref

let get x = x.content

let set (oldref: 'a ft_ref) value =
  oldref.content <- value

let bind (oldref: 'a ft_ref) (f: 'a -> 'b ft_ref) =
  f oldref.content

let () =
  let new_ref = return 42
  in
  let test_f = fun x -> return (89) in
  print_int (get(new_ref));
  print_char '\n';
  set new_ref 45;
  print_int (get(new_ref));
  print_char '\n';
  print_int (get (bind new_ref test_f));
  print_char '\n'