(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tama.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jblondea <jblondea@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/10/13 13:57:01 by jblondea          #+#    #+#             *)
(*   Updated: 2018/10/14 14:02:13 by jblondea         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let cap (value: int) : int = max (min value 100) 0

let read (converter: string -> 'a) (file: Filemonoid.file Try.t) (default: 'a) : ('a * (Filemonoid.file Try.t)) =
  let file = Try.bind file (fun file -> Try.return (Filemonoid.readLine file)) in
  try
    let res = (match file with
      | Success file -> (converter @@ Filemonoid.line file, Try.return file)
      | _ -> (default, file))
    in
    res
  with
    | exn -> (default, Try.Failure exn)

let read_int (file: Filemonoid.file Try.t) (default: int) : (int * (Filemonoid.file Try.t)) =
  read int_of_string file default

let read_bool (file: Filemonoid.file Try.t) (default: bool) : (bool * (Filemonoid.file Try.t)) =
  read bool_of_string file default

let read_string (file: Filemonoid.file Try.t) (default: string) : (string * (Filemonoid.file Try.t)) =
  read (fun x -> x) file default


class tama =
  object (self)
    val _health : int = 100
    val _energy : int = 100
    val _hygiene : int = 100
    val _happiness : int = 100
    val _dead : bool = false
    val _lifeLength : int = 0

    method eat = print_endline @@ "eating...";
      if _dead then self else
      ({< _health = cap (_health + 25); _energy = cap (_energy - 10); _hygiene = cap (_hygiene - 20); _happiness = cap (_happiness + 5) >})#checkDeath
    method thunder = print_endline @@ "thundering...";
      if _dead then self else
      ({< _health = cap (_health - 20); _energy = cap (_energy + 25); _happiness = cap (_happiness - 20) >})#checkDeath
    method bath = print_endline @@ "bathing...";
      if _dead then self else
      ({< _health = cap (_health - 20); _energy = cap (_energy - 10); _hygiene = cap (_hygiene + 25); _happiness = cap (_happiness + 5) >})#checkDeath
    method kill = print_endline @@ "killing...";
      if _dead then self else
      ({< _health = cap (_health - 20); _energy = cap (_energy - 10); _happiness = cap (_happiness + 20) >})#checkDeath

    method get_health = _health

    method get_energy = _energy

    method get_hygiene = _hygiene

    method get_happiness = _happiness

    method get_dead = _dead

    method get_score = _lifeLength

    method undergoTime = print_endline @@ "undergoing 1 more sec...";
      if _dead then (print_endline " .. but already dead."; self) else
      ({< _health = cap (_health - 1); _lifeLength = (_lifeLength + 1) >})#checkDeath

    method die = print_endline "dead.."; {< _dead = true >}

    method checkDeath = if _health <= 0 || _energy <= 0 || _hygiene <= 0 || _happiness <= 0 then self#die else self

    method printSats = print_endline @@
      "{ "
      ^ "health : " ^ string_of_int _health ^ ", "
      ^ "energy : " ^ string_of_int _energy ^ ", "
      ^ "hygiene : " ^ string_of_int _hygiene ^ ", "
      ^ "happiness : " ^ string_of_int _happiness ^ ", "
      ^ "dead : " ^ string_of_bool _dead ^ ", "
      ^ "_lifeLength : " ^ string_of_int _lifeLength ^ " sec"
      ^ " }"
      
    method draw : unit = ()

    method save : unit = 
      let file = Try.return Filemonoid.zero in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.setName "save.itama" file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.openOut file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.write ((string_of_int _health) ^ "\n") file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.write ((string_of_int _energy) ^ "\n") file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.write ((string_of_int _hygiene) ^ "\n") file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.write ((string_of_int _happiness) ^ "\n") file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.write ((string_of_bool _dead) ^ "\n") file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.write ((string_of_int _lifeLength) ^ "\n") file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.closeOut file)) in
      match file with
        | Success x -> ()
        | _ -> print_endline "file save failed!"
    
    method toDefault = 
      {< _health = 100; _energy = 100; _hygiene = 100; _happiness = 100; _dead = false; _lifeLength = 0 >}

    method load = 
      let file = Try.return Filemonoid.zero in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.setName "save.itama" file)) in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.openIn file)) in
      let (health, file) = read_int file 100 in
      let (energy, file) = read_int file 100 in
      let (hygiene, file) = read_int file 100 in
      let (happiness, file) = read_int file 100 in
      let (dead, file) = read bool_of_string file false in
      let (lifeLength, file) = read_int file 0 in
      let file = Try.bind file (fun file -> Try.return (Filemonoid.closeIn file)) in
      let tama = (match file with
        | Success x -> {< _health = health; _energy = energy; _hygiene = hygiene; _happiness = happiness; _dead = dead; _lifeLength = lifeLength >}
        | _ -> (print_endline "file load failed!"); self#toDefault)
      in
      if dead then self#toDefault else
      tama

  end

(* let () =
  let tama : tama = (new tama)#load  in
  tama#printSats;
  let tama = tama#undergoTime in
  tama#printSats;
  let tama = tama#eat in
  tama#printSats;
  let tama = tama#eat in
  tama#printSats;
  let tama = tama#eat in
  tama#printSats;
  let tama = tama#eat in
  tama#printSats;
  let tama = tama#bath in
  tama#printSats;
  let tama = tama#thunder in
  tama#printSats;
  let tama = tama#kill in
  tama#printSats;
  let tama = tama#eat in
  tama#printSats;
  let tama = tama#eat in
  tama#printSats;
  let tama = tama#eat in
  tama#printSats;
  let tama = tama#thunder in
  tama#printSats;
  tama#save; *)

