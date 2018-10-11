class ['a] army (content: 'a list) =
  object
    val mutable _content = content

    initializer print_endline "An army is rising"
    method get_army_length = List.length _content
    method add instance = print_endline "Someone has join the army";_content <- _content @ [instance]
    method delete = _content <- match _content with
      | first::tail -> tail
      | _ -> []
  end