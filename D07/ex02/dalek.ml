class dalek =
  object (self)
    initializer Random.self_init ()
    val _name:string =
      let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
      in
      let rec get_random_chars nb str =
        if(nb = 0) then str
        else
          begin
            Random.self_init ();
            get_random_chars (nb - 1) (str ^ (String.make 1 (String.get chars (Random.int (String.length chars)))))
          end
      in
      "Dalek" ^ get_random_chars 3 ""
    val _hp:int = 100
    val mutable _shield:bool = true
    val _strings = [
      "Explain! Explain!";
      "Exterminate! Exterminate!";
      "I obey!";
      "You are the Doctor! You are the enemy of the Daleks!"
    ]

    method get_string ri =
      let rec find strings i = match strings with 
        | str::tail when i = ri -> str
        | str::tail-> find tail (i + 1)
        | []-> ""
      in
      find _strings 0
    method talk = print_endline (self#get_string (Random.int 4))
    method die = print_endline "Emergency Temporal Shift!"
    method exterminate (people:People.people) = people#die; _shield <- not _shield
    method to_string = "name: " ^ _name ^ "; hp: " ^ string_of_int _hp ^ "; shield: " ^ string_of_bool _shield 
  end