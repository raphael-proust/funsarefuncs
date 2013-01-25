(* Default value type *)
module type VAL = sig
	type t
	val v: t
end
module type TEQ = sig (*to propagate type equality*)
	type t
end

(*UNITS *)
module type UNIT = VAL with type t = unit

module Unit : UNIT = struct type t = unit let v : t = () end
module Ignore (V : sig end) : UNIT = Unit
module Noop (V: UNIT) : UNIT = Unit


(*PRODUCTS*)
module type PROD = sig
	module A : VAL
	module B : VAL
end

module First (P: PROD) : (VAL with type t = P.A.t) = P.A
module Second (P: PROD) : (VAL with type t = P.B.t) = P.B
module Pair (A: VAL) (B: VAL) : (PROD with type A.t = A.t and type B.t = B.t) =
	struct
		module A = A
		module B = B
	end


(* Note! We can't invert the order of parameters. In particular
module Unzip =
	functor (F: functor (A: VAL) -> functor (B:VAL) -> VAL) ->
	functor (P:PROD with type a = A.t and type b = B.t) ->
	F (First (P)) (Second (P))
doesn't work. The A (and thus A.t) is not bound in PROD's type constraints.
*)
(* Note! We can't propagate type equality between the result of F and the result of Unzip. Using functions we write:
unzip : ('a -> 'b -> 'c) -> ('a, 'b) pair -> 'c
where the final 'c is equal to the initial one. Using functors this is not possible.

We work around this issue by introducing TEQ.
*)
module Unzip
	(T:TEQ) (P:PROD)
	(F: functor (A: VAL with type t = P.A.t) ->
		functor (B:VAL with type t = P.B.t) ->
			VAL with type t = T.t)
	: (VAL with type t = T.t) =
		F (First (P)) (Second (P))

module Zip
	(T:TEQ) (A: VAL) (B : VAL)
	(F: functor (P:PROD with type A.t = A.t and type B.t = B.t) -> VAL with type t = T.t)
	: (VAL with type t = T.t) =
		F (Pair (A) (B))

module Both
	(TA:TEQ) (TB:TEQ) (P: PROD)
	(FA: functor (A: VAL with type t = P.A.t) -> VAL with type t = TA.t)
	(FB: functor (B: VAL with type t = P.B.t) -> VAL with type t = TB.t)
	: (PROD with type A.t = TA.t and type B.t = TB.t) =
		Pair (FA (First (P))) (FB (Second (P)))



(*SUMS*)
module type SUM =
	functor (TL:TEQ) -> functor (TR:TEQ) ->
	functor (T:TEQ) ->
	functor (L: functor (V: VAL with type t = TL.t) -> (VAL with type t = T.t)) ->
	functor (R: functor (V: VAL with type t = TR.t) -> (VAL with type t = T.t)) ->
	(VAL with type t = T.t)

module Left
	(TL:TEQ)
	(V: VAL with type t = TL.t)
	= functor (TTL:TEQ with type t = TL.t) -> functor (TR:TEQ) -> functor (T:TEQ) ->
		functor (L: functor (V: VAL with type t = TTL.t) -> (VAL with type t = T.t)) ->
		functor (R: functor (V: VAL) -> (VAL with type t = T.t)) ->
			L (V)
module Right
	(TR:TEQ)
	(V: VAL with type t = TR.t)
	= functor (TL:TEQ) -> functor (TTR:TEQ with type t = TR.t) -> functor (T:TEQ) ->
		functor (L: functor (V: VAL) -> (VAL with type t = T.t)) ->
		functor (R: functor (V: VAL with type t = TTR.t) -> (VAL with type t = T.t)) ->
			R (V)


module Either
	(TL:TEQ) (TR:TEQ) (T:TEQ)	
	(S: SUM)
	(FL: functor (V:VAL with type t = TL.t) -> (VAL with type t = T.t))
	(FR: functor (V:VAL with type t = TR.t) -> (VAL with type t = T.t))
	= (S (TL) (TR) (T)) (FL) (FR)
