module Try =
  struct
    type 'a t = Success of 'a | Failure of exn
    let return (value: 'a) = Success value
    let bind (mon:'a t) f =
      try
        Success (f mon)
      with err -> Failure err
      
  end

let () =
  let t = Try.return (15 / 5) in
  ignore(Try.bind t (fun x -> Try.return(x / 2)))