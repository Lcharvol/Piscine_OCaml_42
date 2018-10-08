module Tictactoe =
struct 

  type player = Player1 | Player2 | None

  type cell = (int * bool * player * char)

  type allplayer = (string * string)

  let can_i_fill grid x y =
    let rec check_grid grid x y =
      match grid with
      | [] -> true
      | head :: next ->
        let (pos, is_empty, _, _) = head in
        if (((pos / 3) = x) && (pos mod 3) = y)
        then
          begin 
            is_empty end
        else check_grid next x y
    in
    if ((x < 0) || (x > 2) || (y < 0) || (y > 2))
    then false
    else (check_grid grid x y)

  let player_char player (p1, p2)=
    match player with
    | Player1 -> String.get p1 0
    | Player2 -> String.get p2 0
    | None -> '-'

  let ogrid player = (0, false, player, '/') :: (1, false, player, '-') :: (2, false, player, '\\') :: (3, false, player, '|') :: (4, false, player, ' ') :: (5, false, player, '|') :: (6, false, player, '\\') :: (7, false, player, '-') :: (8, false, player, '/') :: []

  let xgrid player = (0, false, player, '\\') :: (1, false, player, ' ') :: (2, false, player, '/') :: (3, false, player, ' ') :: (4, false, player, 'X') :: (5, false, player, ' ') :: (6, false, player, '/') :: (7, false, player, ' ') :: (8, false, player, '\\') :: []

  let grid_of player allplayer=
    let rec getgrid pos =
      match pos with
      | 9 -> []
      | _ -> (pos, (if player <> None then false else true), player, (player_char player allplayer)) :: getgrid (pos + 1)
    in
    getgrid 0

  let grid_of_winner player (p1, p2) =
    let get_grid str =
      if ((String.get str 0) = 'X' || (String.get str 0) = 'x')
      then xgrid player
      else if ((String.get str 0) = 'o' || (String.get str 0) = 'O')
      then ogrid player
      else (grid_of player (p1, p2)) in
    match player with
    | Player1 -> get_grid p1
    | _ -> get_grid p2

  let check_win grid =
    match grid with
    | [] -> None
    | (_, _, Player1, _) :: (_, _, Player1, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player1
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, Player1, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player1
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, Player1, _) :: (_, _, Player1, _) :: [] -> Player1
    | (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player1
    | (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: [] -> Player1
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: [] -> Player1
    | (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: [] -> Player1
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, Player1, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player1
    | (_, _, Player2, _) :: (_, _, Player2, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player2
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, Player2, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player2
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, Player2, _) :: (_, _, Player2, _) :: [] -> Player2
    | (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player2
    | (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: [] -> Player2
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: [] -> Player2
    | (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: [] -> Player2
    | (_, _, _, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, Player2, _) :: (_, _, _, _) :: (_, _, _, _) :: [] -> Player2
    | head :: tail -> None

  let fill_grid grid x y player allplayer =
    let rec filler grid x y =
      match grid with
      | [] -> []
      | head :: next ->
        let (pos, _, _, _) = head in
        if (((pos / 3) = x) && ((pos mod 3) = y))
        then (pos, false, player, (player_char player allplayer)) :: next
        else (head :: (filler next x y))
    in
    let grid = filler grid x y in
    match (check_win grid) with
    | Player1 | Player2 -> grid_of_winner player allplayer
    | None -> grid

  let rec list_of_grid n =
    match n with
    | 0 -> []
    | _ -> ((grid_of None ("", "")) :: list_of_grid (n -1))

  let check_big_grid bgrid x y=
    let rec browser grid n x y =
      match grid with
      | [] -> false
      | head :: next ->
        match n with
        | 0 -> (can_i_fill head x y)
        | _ -> browser next (n - 1) x y
    in
    browser bgrid (((x - 1) / 3) + (((y - 1) / 3) * 3)) ((y - 1) mod 3) ((x - 1) mod 3) 

  let fill_big_grid bgrid x y player allplayer =
    let rec browser grid n x y =
      match grid with
      | [] -> []
      | head :: next ->
        match n with
        | 0 -> ((fill_grid head x y player allplayer) :: next)
        | _ -> (head :: browser next (n - 1) x y)
    in
    browser bgrid (((x - 1) / 3) + (((y - 1) / 3) * 3)) ((y - 1) mod 3) ((x - 1) mod 3) 


  let print_grids grids allplayer =
    let rec print_grids_rec grids grid1 grid2 grid3 =
      let print_player c = print_char c; print_char ' '
      in
      let print_line grid nb =
        let rec print_line_rec grid pos = match grid with
          | (_, _, player, c)::tail -> if(ceil(float_of_int(pos / 3)) = (float_of_int nb))
            then
              print_player c;
            print_line_rec tail (pos + 1)
          | [] -> ()
        in
        print_line_rec grid 0
      in
      let print_separator () = print_endline "---------------------"
      in
      let pipe () = print_string "| "
      in
      if(grid1 <= 8)
      then match grids with
        | first::second::third::tail -> print_line first 0; pipe(); print_line second 0; pipe(); print_line third 0; print_char '\n';
          print_line first 1; pipe(); print_line second 1; pipe(); print_line third 1; print_char '\n';
          print_line first 2; pipe(); print_line second 2; pipe(); print_line third 2; print_char '\n';
          if(grid1 < 6) then print_separator();
          print_grids_rec tail (grid1 + 3) (grid2 + 3) (grid3 + 3);
        | _ -> ()
    in
    print_char '\n';
    print_grids_rec grids 0 1 2;
    print_char '\n'

  let big_check_win grid =
    let rec big_to_little grid =
      match grid with
      | [] -> []
      | head :: next -> (0, true, check_win head, ' ') :: big_to_little next in
    check_win (big_to_little grid)

  let is_numeric str =
    let rec is_num n =
      if n = 0
      then true
      else let is_digit = function '0' .. '9' -> true | _ -> false in
        match is_digit (String.get str n)  with
        | true -> true && is_num (n - 1)
        | false -> false
    in
    is_num ((String.length str) - 1)

  let get_row_col () =
    let row_col = read_line () in
    let rec trim l =
      match l with
      | [] -> []
      | head :: next ->
        match head with
        | "" -> trim next
        | _ -> head :: (trim next)
    in
    trim (String.split_on_char ' ' row_col)

  let print_winner player =
    match player with
    | Player1 -> print_endline "Winner is Player1"
    | Player2 -> print_endline "Winner is Player2"
    | None -> print_endline "Nobody won the game"

  let rec check_free_space grid =
    let rec check_little_grid_space lgrid =
      match lgrid with
      | [] -> false
      | (_, is_empty, _, _) :: next -> (is_empty || (check_little_grid_space next)) in
    match grid with
    | [] -> false
    | head :: next -> ((check_little_grid_space head) || check_free_space next)

  let tictactoe player1 player2 =
    let rec game grid currentplayer nextplayer =
      let print_current_player () =
        match currentplayer with
        | Player1 -> print_endline (player1 ^ "'s turn to play")
        | _ -> print_endline (player2 ^ "'s turn to play") in
      let rec player_turn () =
        let row_col = get_row_col () in
        let rec row_col_tup str =
          match str with
          | [] -> ((-1), (-1))
          | head :: next :: [] ->
            begin
              match (is_numeric head) && (is_numeric next) with
              | true -> (int_of_string head, int_of_string next)
              | false -> ((-1),(-1))
            end
          | head :: next -> ((-1),(-1))
        in
        let (row, col) = row_col_tup row_col in
        if (row = (-1) || col = (-1))
        then
          begin
            print_endline "Incorrect format.";
            player_turn ()
          end
        else if check_big_grid grid row col
        then 
          fill_big_grid grid row col currentplayer (player1, player2)
        else
          begin
            print_endline "Illegal move.";
            player_turn ()
          end
      in
      print_grids grid (player1, player2);
      print_current_player ();
      let grid = player_turn () in
      let winner = big_check_win grid in
      if winner <> None
      then 
        begin
          print_grids grid (player1, player2);
          winner
        end
      else if check_free_space grid
      then game grid nextplayer currentplayer
      else
        begin
          print_grids grid (player1, player2);
          None
        end
    in
    print_winner (game (list_of_grid 9) Player1 Player2)

end
