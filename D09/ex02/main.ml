module type MONOID =
  sig
    type element
    val zero1 : element
    val zero2 : element
    val mul : element -> element -> element
    val add : element -> element -> element
    val div : element -> element -> element
    val sub : element -> element -> element
  end

module INT =
  struct
    type element = int
    let zero1 = 0
    let zero2 = 1
    let mul = ( * )
    let add = ( + )
    let div = ( / )
    let sub = ( - )
  end

module FLOAT =
  struct
    type element = float
    let zero1 = 0.0
    let zero2 = 1.0
    let mul = ( *. )
    let add = ( +. )
    let div = ( /. )
    let sub = ( -. )
  end

module type CALC =
  functor (M : MONOID) ->
    sig
      val add : M.element -> M.element -> M.element
      val sub : M.element -> M.element -> M.element
      val mul : M.element -> M.element -> M.element
      val div : M.element -> M.element -> M.element
      val power : M.element -> int -> M.element
      val fact : M.element -> M.element
    end

module Calc : CALC =
  functor (Module : MONOID) ->
    struct
      let add a b = Module.add a b
      let sub a b = Module.sub a b
      let mul a b = Module.mul a b
      let div a b = Module.div a b
      let power a b =
        let rec power_rec b =
          if(b = 0) then Module.zero2
          else Module.mul a (power_rec (b - 1))
        in
        power_rec b
      let fact a =
        let rec fact_rec a =
          if(a = Module.zero1) then Module.zero2
          else Module.mul a (fact_rec (Module.sub a Module.zero2))
        in
        fact_rec a
    end


module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)
let () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.sub 5.0 9.4));
  print_endline (string_of_int (Calc_int.sub 5 9));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_float (Calc_float.fact 5.0));
  print_endline (string_of_int (Calc_int.fact 11));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
