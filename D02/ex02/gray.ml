let print_gray s s2 =
	let rec pr i size str=
		if i = size then
			str
		else if (String.get s i) <> (String.get s2 i) then
			pr (i + 1) size (str ^ "1")
		else
			pr (i + 1) size (str ^ "0")
	in print_char ' '; print_string (pr 0 (String.length s) "")

let gray n =
	let rec gray_recursive n str  =
    if n = 0 then
			print_gray str ("0" ^ str)
		else
      let c = (n - 1) in
      gray_recursive c (str ^ "0") ;
			gray_recursive c (str ^ "1") 
  in gray_recursive n "";
  print_char '\n'

let () =
  gray 0;
  gray 1;
  gray 2;
  gray 3