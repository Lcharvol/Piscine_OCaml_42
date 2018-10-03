type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let square_size = 40

let win_width = 2500

let win_height = 1200


let size tree =
    let rec size_rec tree acc = match tree with
        | Nil -> acc
        | Node(a, b, c) -> (size_rec b acc) + (size_rec c acc) + 1
    in
    size_rec tree 0

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

let draw_square x y size =
  let center = size / 2 in
  Graphics.moveto (x - center) (y - center);
  Graphics.lineto (x - center) ((y - center) + size);
  Graphics.lineto ((x - center) + size) ((y - center) + size);
  Graphics.lineto ((x - center) + size) (y - center);
  Graphics.lineto (x - center) (y - center);
  Graphics.moveto x y

let draw_text x y size text =
    draw_square x y size;
    Graphics.moveto (x - (square_size / 4)) (y - 5);
    Graphics.draw_string text;
    Graphics.moveto x y

let draw_tree_node tree x y height_space width_space = match tree with
    | Nil -> draw_text x y square_size "Nil"
    | Node(a, b, c) ->  let center = square_size / 2 in
                        draw_text x y square_size a;
                        Graphics.moveto (x + center) y;
                        Graphics.lineto (x +  width_space - center) (y - height_space);
                        Graphics.moveto (x + center) y;
                        Graphics.lineto (x +  width_space - center) (y + height_space)

let draw_tree tree =
    let rec draw_tree_rec tree x y height_space width_space = match tree with
        | Nil -> draw_tree_node Nil x y height_space width_space;
        | Node(a, b, c) ->  let new_height_space =
                                if ((height_space / 2)) > square_size
                                    then (height_space / 2)
                                else square_size
                            in
                            let new_width_space =
                                if (4 * (width_space / 5)) > 100
                                    then 4 * (width_space / 5)
                                else 100
                            in
                            draw_tree_node (Node(a, b, c)) x y height_space width_space;
                            draw_tree_rec b (x + width_space) (y + height_space) new_height_space new_width_space;
                            draw_tree_rec c (x + width_space) (y - height_space) new_height_space new_width_space

    in
    let height_space = 100 * height tree in
    let width_space = 125 + 50 * height tree in
    draw_tree_rec tree 250 (win_height / 2) height_space width_space

let () =
    let tree =  Node ("Test",  Node ("Test",  Node ("Test", Nil, Nil),  Node ("Test", Nil, Nil)),  Node ("Test",  Node ("Test", Nil, Nil),  Node ("Test", Nil, Nil))) in
    print_int (size tree);
    print_char '\n';
    print_int (height tree);
    print_char '\n';
    Graphics.open_graph (" " ^ (string_of_int win_width) ^ "x" ^ (string_of_int win_height));
    Graphics.set_window_title "gardening";
    draw_tree tree;
    print_char (Graphics.read_key())