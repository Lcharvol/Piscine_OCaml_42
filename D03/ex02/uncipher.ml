let unrot42 str =
  let isAlpha c = c >= 'a' && c <= 'z'
  in
  let isCapitalizedAlpha c = c >= 'A' && c <= 'Z'
  in
  let rotateChar c =
    if isAlpha c
      then
        if (int_of_char c - 16) < 97
          then char_of_int (122 + (int_of_char c - 16 - 96))
          else char_of_int (int_of_char c - 16)
      else 
        if isCapitalizedAlpha c
          then
            if (int_of_char c - 16) < 65
              then char_of_int (90 + (int_of_char c - 16 - 64))
              else char_of_int (int_of_char c - 16)
          else c
  in
  String.map rotateChar str

let uncaesar n str =
  let real_rot = n mod 26
  in
  let isAlpha c = c >= 'a' && c <= 'z'
  in
  let isCapitalizedAlpha c = c >= 'A' && c <= 'Z'
  in
  let rotateChar c =
    if isAlpha c
      then
        if (int_of_char c - real_rot) < 97
          then char_of_int (122 + (int_of_char c - real_rot - 96))
          else char_of_int (int_of_char c - real_rot)
      else 
        if isCapitalizedAlpha c
          then
            if (int_of_char c - real_rot) < 65
              then char_of_int (90 + (int_of_char c - real_rot - 64))
              else char_of_int (int_of_char c - real_rot)
          else c
  in
  String.map rotateChar str

let xor key str =
  if (key > 0) then
    let xorChar c = char_of_int((int_of_char c) lxor (key mod 255)) in
    String.map xorChar str
  else
    str

let rec ft_uncrypt str l = match l with
  | f::tail -> ft_uncrypt (f str) tail
  | [] -> str

let () =
	let s1 = unrot42 "Xubbe meht !"
  in
  print_string "s1: ";
  print_string s1;
  print_char '\n';
	let s2 = uncaesar 10 "Rovvy gybn !"
  in
  print_string "s2: ";
  print_string s2;
  print_char '\n';
	let s3 = xor 2 "Jgnnm\"umpf\"#"
  in
  print_string "s3: ";
  print_string s3;
  print_char '\n';

	let s4 = ft_uncrypt "Jwnnm*}mfv*+" [uncaesar 8; xor 10]
  in
  print_string "s4: ";
  print_string s4; 
  print_char '\n'