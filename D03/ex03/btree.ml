type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let is_bst tree =
  let rec is_sorted l = match l with
    | first::second::tail ->  if(first >= second)
                                then false
                                else is_sorted tail
    | alone -> true
    
  in
  let rec get_list_from_tree tree = match tree with
    | Nil -> []
    | Node(a, Node(b, c, d), Node(e, f, g)) ->  (get_list_from_tree (Node(b, c, d))) @ [a] @
                                                (get_list_from_tree (Node(e, f, g)))
    | Node(a, l, Nil) -> [a] @ get_list_from_tree l
    | Node(a, Nil, r) -> [a] @ get_list_from_tree r
  in
  is_sorted (get_list_from_tree tree)

let rec is_perfect tree = match tree with
  | Nil -> true
  | Node(a, Nil, Nil) -> true
  | Node(a, Nil, b) | Node(a, b, Nil) -> false
  | Node(a, b, c) -> is_perfect b && is_perfect c

let height tree =
  let rec height_rec tree acc = match tree with
      |Nil -> acc
      |Node(a, b, c) ->   let height_left = height_rec b acc in
                          let height_right = height_rec c acc in
                          if height_left > height_right
                              then height_left + 1
                          else height_right + 1
  in
  height_rec tree 0

let rec is_balanced tree = match tree with
  | Nil -> true
  | Node(a, Nil, Nil) -> true
  | Node(a, Nil, b) | Node(a, b, Nil) -> if((height b) > 1) then false else true
  | Node(a, b, c) ->  if((abs ((height b) - (height c))) <= 1)
                        then is_balanced b && is_balanced c
                      else false

let rec search_bst value tree = match tree with
  | Nil -> false
  | Node(a, Nil, Nil) -> if(a = value) then true else false
  | Node(a, Nil, b) | Node(a, b, Nil) ->  if(a = value)
                                            then true
                                          else search_bst value b
  | Node(a, b, c) ->  if(a = value)
                        then true
                      else (search_bst value b) || (search_bst value c)

let () =
  if(( is_bst (
    Node (20,
      Node (8, 
        Node (4,
          Nil,
          Nil
        ),
        Node (12,
          Node (10, Nil, Nil),
          Node (19, Nil, Nil)
        )
      ),
      Node (22, Nil, Nil)
    )
  )) = true)
    then print_endline "is_bst: True"
    else print_endline "is_bst: False";
  if(( is_perfect (
    Node (20,
      Node (8, 
        Node (4,
          Nil,
          Nil
        ),
        Node (12,
          Node (10, Nil, Nil),
          Node (19, Nil, Nil)
        )
      ),
      Node (22, Nil, Nil)
    )
  )) = true)
    then print_endline "is_perfect: True"
    else print_endline "is_perfect: False";
  if((is_balanced (
    Node (20,
      Node (8, 
        Nil,
        Nil
      ),
      Node (22, Nil, Nil)
    )
  )) = true)
    then print_endline "is_balanced: True"
    else print_endline "is_balanced: False"
    ;
  if((search_bst 8 (
    Node (20,
      Node (8, 
        Nil,
        Nil
      ),
      Node (22, Nil, Nil)
    )
  )) = true)
    then print_endline "search_bst: True"
    else print_endline "search_bst: False"