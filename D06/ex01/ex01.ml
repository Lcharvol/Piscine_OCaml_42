module HashString =
  struct
    type t = String.t
    let equal str1 str2 = (String.compare str1 str2 = 0)
    let hash str =
      let rec hash_rec str ac i =
        if i = 0 then ac
        else
          hash_rec str ((ac lsl 5) + ac + i) (i - 1)
      in
      hash_rec str 0 (String.length str - 1)
  end

module StringHashtbl = Hashtbl.Make (HashString)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht