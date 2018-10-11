let () =
  let soldier1 = new People.people "Soldier 1" in
  let soldier2 = new People.people "Soldier 2" in
  let soldier3 = new People.people "Soldier 3" in
  let captain = new People.people "Captain" in
  let doctor_who = new Doctor.doctor ("Who") 24 captain in
  let doctor_who_bis = new Doctor.doctor ("Who Bis") 24 captain in
  let dalek1 = new Dalek.dalek in
  let dalek2 = new Dalek.dalek in
  let soldier_army = new Army.army [soldier1] in
  let doctor_army = new Army.army [doctor_who] in
  let dalek_army = new Army.army [dalek1] in

  (* Initial State *)
  print_string "Length of soldier_army: ";
  print_int soldier_army#get_army_length;
  print_char '\n';
  print_string "Length of doctor_army: ";
  print_int doctor_army#get_army_length;
  print_char '\n';
  print_string "Length of dalek_army: ";
  print_int dalek_army#get_army_length;
  print_char '\n';

  (* Soldier army *)
  soldier_army#add soldier2;
  print_string "Length of soldier_army: ";
  print_int soldier_army#get_army_length;
  print_char '\n';
  soldier_army#add soldier3;
  print_string "Length of soldier_army: ";
  print_int soldier_army#get_army_length;
  print_char '\n';
  soldier_army#delete;
  print_string "Length of soldier_army: ";
  print_int soldier_army#get_army_length;
  print_char '\n';
  soldier_army#delete;
  print_string "Length of soldier_army: ";
  print_int soldier_army#get_army_length;
  print_char '\n';

  (* Doctor army *)
  doctor_army#add doctor_who_bis;
  print_string "Length of doctor_army: ";
  print_int doctor_army#get_army_length;
  print_char '\n';
  doctor_army#delete;
  print_string "Length of doctor_army: ";
  print_int doctor_army#get_army_length;
  print_char '\n';
  doctor_army#delete;
  print_string "Length of doctor_army: ";
  print_int doctor_army#get_army_length;
  print_char '\n';

  (* Dalek army *)
  dalek_army#add dalek2;
  print_string "Length of dalek_army: ";
  print_int dalek_army#get_army_length;
  print_char '\n';
  dalek_army#delete;
  print_string "Length of dalek_army: ";
  print_int dalek_army#get_army_length;
  print_char '\n';
  dalek_army#delete;
  print_string "Length of dalek_army: ";
  print_int dalek_army#get_army_length;
  print_char '\n';
  dalek_army#delete;
  print_string "Length of dalek_army: ";
  print_int dalek_army#get_army_length;
  print_char '\n'