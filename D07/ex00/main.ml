let () =
  let people = new People.people ("Lucas")
  in
  print_endline people#to_string;
  people#talk;
  people#die
