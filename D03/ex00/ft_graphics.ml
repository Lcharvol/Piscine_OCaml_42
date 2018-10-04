type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let square_size = 50

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

let draw_tree_node tree =
  let rec draw_tree_node_rec tree x y = match tree with
  | Nil -> draw_text x y square_size "Nil"
  | Node(a, b, c) ->  let center = square_size / 2 in
                      draw_text x y square_size a;
                      Graphics.moveto (x + center) y;
                      Graphics.lineto (x + square_size * 2) (y - square_size);
                      draw_tree_node_rec b (x + (square_size * 2) + center) (y - square_size);
                      Graphics.moveto (x + center) y;
                      Graphics.lineto (x + square_size * 2) (y + square_size);
                      draw_tree_node_rec b (x + (square_size * 2) + center) (y + square_size)
  in
  draw_tree_node_rec tree 250 250



let _ =
  Graphics.open_graph "";
  Graphics.set_window_title "ft_graphics";
  draw_tree_node (Node ("Test", Nil, Nil));
  print_char (Graphics.read_key())