class doctor name age people =
  object
    val _name:string = name
    val _age:int = age
    val _sidekick:People.people = people 
    val mutable _hp = 100

    initializer print_endline ("Doctor " ^ _name ^ " has been summoned")
    method travel_in_time (start:int) (arrival:int) = print_endline 
    "    _______(_@_)_______
    | POLICE      BOX |
    |_________________|
     | _____ | _____ |
     | |###| | |###| |
     | |###| | |###| |   
     | _____ | _____ |   
     | || || | || || |
     | ||_|| | ||_|| |  
     | _____ |$_____ |  
     | || || | || || |  
     | ||_|| | ||_|| | 
     | _____ | _____ |
     | || || | || || |   
     | ||_|| | ||_|| |         
     |       |       |        
     *****************"
    method private regenerate = _hp <- 100
    method talk = print_endline "Hi! I’m the Doctor!"
    method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method to_string = "name: " ^ _name ^ "; age: " ^ (string_of_int _age) ^ "; hp: " ^ (string_of_int _hp) ^ "; sidekick: " ^ _sidekick#to_string
  end