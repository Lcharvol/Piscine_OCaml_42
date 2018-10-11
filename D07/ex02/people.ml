class people name =
  object
    val _name:string = name
    val mutable _hp = 100

    initializer print_endline "A people instance has been created"
    method to_string = _name ^ " has " ^ string_of_int _hp ^ " hp."
    method die = print_endline "Aaaarghh!"
    method talk = print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
  end