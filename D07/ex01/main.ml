let () =
  let doctor = new Doctor.doctor ("Dr. Who") 24 (new People.people ("Laurent")) in
  doctor#talk;
  doctor#travel_in_time 2018 1995;
  doctor#use_sonic_screwdriver;
  print_endline doctor#to_string;