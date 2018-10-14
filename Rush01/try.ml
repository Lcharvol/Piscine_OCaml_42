(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   try.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jblondea <jblondea@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/10/12 15:29:37 by jblondea          #+#    #+#             *)
(*   Updated: 2018/10/13 19:19:21 by jblondea         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* module Try =
  struct *)
    type 'a t = Success of 'a | Failure of exn
    exception FilterFail

    let return x = Success x

    let bind (value: 'a t) (f: 'a -> 'b t) : 'b t = match value with
      | Success (x) -> (try (f x) with | x -> Failure x)
      | x -> x
    
    let recover (value: 'a t) (f: exn -> 'a t) : 'a t = match value with
      | Failure x -> f x
      | x -> x
    
    let filter (value: 'a t) (f: 'a -> bool) : 'a t = match value with
      | Success x -> if f x then Success x else Failure FilterFail
      | x -> x
    
    let flatten (value: 'a t t) : 'a t = match value with
      | Success x -> (match x with 
        | Success a -> Success a
        | y -> y)
      | Failure y -> Failure y
  (* end *)

(* module OutChannelTry =
  functor (Elem) -> Try *)


  (* exception Foo of string *)

(* let () = *)
  (* let file = Try.return FileMonoid.zero in
  let file = Try.bind file (fun file -> Try.return ("save.itama", FileMonoid.inChannel file, FileMonoid.outChannel file, "")) in
  let file = Try.bind file (fun file -> Try.return (FileMonoid.openOut file)) in
  let file = Try.bind file (fun file -> Try.return (FileMonoid.write "lol\n" file)) in
  let file = Try.bind file (fun file -> Try.return (FileMonoid.closeOut file)) in
  (match file with
    | Success x -> print_endline @@ "all good!"
    | Failure x -> print_endline @@ "failed!");
  let file = Try.bind file (fun file -> Try.return (FileMonoid.openIn file)) in
  let file = Try.bind file (fun file -> Try.return (FileMonoid.readLine file)) in
  let content = (FileMonoid.line (match file with | Success x -> x)) in
  print_endline content; *)
  (* (try 
    close_out_noerr stdout
    (* raise (Foo "lol") *)
  with
    | _ -> print_endline "hum.."); *)
  (* let file = Try.bind file (fun file -> Try.return (FileMonoid.closeIn file)) in
  (match file with
    | Success x -> print_endline @@ "all good!"
    | Failure x -> print_endline @@ "failed!");
  print_endline "end";
  () *)
  