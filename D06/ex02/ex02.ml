module type PAIR =
  sig
    val pair : (int * int)
  end

module type VAL =
  sig
    val x : int
  end

module type MAKEPROJECTION =
  functor (Pair : PAIR) -> VAL

module Pair : PAIR =
  struct
    let pair = (21, 42)
  end

module MakeFst : MAKEPROJECTION =
  functor (IntPair : PAIR) ->
    struct
      let x = Pervasives.fst IntPair.pair
    end

module MakeSnd : MAKEPROJECTION =
  functor (IntPair : PAIR) ->
    struct
      let x = Pervasives.snd IntPair.pair
    end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x