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
    then print_endline "True"
    else print_endline "False"