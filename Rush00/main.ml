let rec main players =
  let rec check_length players =
    match players with
    | [] -> true
    | head :: next -> ((String.length head) <> 0 && check_length next)
  in
  let check_name () =
    match players with
    | [] -> true;
    | head :: next :: [] -> ((String.get head 0) <> '-' && (String.get next 0) <> '-' && (String.get head 0) <> (String.get next 0));
    | head :: next -> (String.get head 0) <> '-';
  in
  let launchgame () =
    match players with
    | [] -> Tictactoe.Tictactoe.tictactoe "X" "O";
    | head :: next :: [] -> Tictactoe.Tictactoe.tictactoe head next;
    | head :: [] -> Tictactoe.Tictactoe.tictactoe head (match String.get head 0 with | 'x' | 'X' -> "O" | _ -> "X");
    | head :: next -> print_string "Error";
  in
  if (check_length players && check_name ()) = false
  then print_endline "./tictactoe [player1] [player2]\nPlease make sure first letter are different from each other."
  else
    begin
      launchgame ();
      let rec newmatch () =
        print_endline "New match ? Yes/No";
        match (read_line ()) with
        | "y" | "yes" | "Yes" -> main players
        | "n" | "no" | "No" -> ()
        | _ -> newmatch ()
      in
      newmatch ()
    end

let () =
  let argv = Array.to_list Sys.argv in
  if (List.length argv) <= 3
  then main (List.tl argv)
  else print_endline "./tictactoe [player1] [player2]"
