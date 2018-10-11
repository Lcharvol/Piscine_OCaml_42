let () =
  let dalek = new Dalek.dalek in
  let people = new People.people ("Laurent") in
  let doctor = new Doctor.doctor ("Dr. Who") 24 people in
  print_endline dalek#to_string;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  dalek#talk;
  doctor#talk;
  doctor#travel_in_time 2018 1995;
  doctor#use_sonic_screwdriver;
  print_endline doctor#to_string;
  dalek#exterminate people;
  print_endline dalek#to_string;
  dalek#exterminate people;
  print_endline dalek#to_string;
  dalek#exterminate people;
  print_endline dalek#to_string;
  dalek#die