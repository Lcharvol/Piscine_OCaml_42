let rot42 str =
  let isAlpha c = c >= 'a' && c <= 'z'
  in
  let isCapitalizedAlpha c = c >= 'A' && c <= 'Z'
  in
  let rotateChar c =
    if isAlpha c
      then
        if (int_of_char c + 16) > 122
          then char_of_int (97 + (int_of_char c + 16 - 123))
          else char_of_int (int_of_char c + 16)
      else 
        if isCapitalizedAlpha c
          then
            if (int_of_char c + 16) > 90
              then char_of_int (65 + (int_of_char c + 16 - 91))
              else char_of_int (int_of_char c + 16)
          else c
  in
  String.map rotateChar str

let caesar n str =
  let real_rot = n mod 26
  in
  let isAlpha c = c >= 'a' && c <= 'z'
  in
  let isCapitalizedAlpha c = c >= 'A' && c <= 'Z'
  in
  let rotateChar c =
    if isAlpha c
      then
        if (int_of_char c + real_rot) > 122
          then char_of_int (97 + (int_of_char c + real_rot - 123))
          else char_of_int (int_of_char c + real_rot)
      else 
        if isCapitalizedAlpha c
          then
            if (int_of_char c + real_rot) > 90
              then char_of_int (65 + (int_of_char c + real_rot - 91))
              else char_of_int (int_of_char c + real_rot)
          else c
  in
  String.map rotateChar str

let xor key str =
  if (key > 0) then
    let xorChar c = char_of_int((int_of_char c) lxor (key mod 255)) in
    String.map xorChar str
  else
    str

let rec ft_crypt str l = match l with
  | f::tail -> ft_crypt (f str) tail
  | [] -> str



let () =
	let s1 = rot42 "Hello word !"
  in
  print_string "s1: ";
  print_string s1;
  print_char '\n';
	let s2 = caesar 10 "Hello word !"
  in
  print_string "s2: ";
  print_string s2;
  print_char '\n';
	let s3 = xor 2 "Hello word !"
  in
  print_string "s3: ";
  print_string s3;
  print_char '\n';

	let s4 = ft_crypt "Hello word !" [xor 10; caesar 8]
  in
  print_string "s4: ";
  print_string s4; 
  print_char '\n'