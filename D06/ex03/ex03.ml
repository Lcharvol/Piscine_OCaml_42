module type FIXED = sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig val bits : int end

module type MAKE =
	functor (Fract : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
	functor (Fract : FRACTIONNAL_BITS) ->
		struct
      type t = int
			let of_int x = x lsl Fract.bits
			let of_float x = int_of_float (floor (0.5 +. x *. (float_of_int (of_int 1))))
			let to_float t = (float_of_int t) /. (2. ** float_of_int(Fract.bits))
			let to_int t = t lsr Fract.bits
      let to_string t = string_of_float (to_float t)
      let zero = of_int 0
			let one = of_int 1
			let succ t = t + 1
			let pred t = t - 1
			let min = Pervasives.(min)
			let max = Pervasives.(max)
			let gth = Pervasives.(>)
			let lth= Pervasives.(<)
			let gte = Pervasives.(>=)
			let lte = Pervasives.(<=)
			let eqp = Pervasives.(==)
			let eqs = Pervasives.(=)
			let add = Pervasives.(+)
			let sub = Pervasives.(-)
			let mul = Pervasives.( * )
			let div = Pervasives.(/)
			let rec foreach a b c =
        if a > b
          then ()
        else
          begin
            c a;
            foreach (a + 1) b c
          end
		end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))