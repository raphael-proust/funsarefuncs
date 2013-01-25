






(* type unit = unit *)

let unit = ()
let ignore _ = ()
let noop () = ()


type ('a, 'b) prod = ('a * 'b)

let first (x, _) = x
let second (_, y) = y
let pair x y = (x, y)
let unzip f p = f (first p) (second p)
let zip f x y = f (pair x y)
let both fx fy p = pair (fx (first p)) (fy (second p))


type ('a, 'b) sum =
	| Left of 'a
	| Right of 'b

let left x = Left x
let right y = Right y
let either fx fy = function
	| Left x -> fx x
	| Right y -> fy y


type 'a list = List of (('a, 'a list) prod, unit) sum

let list l = List l
let cons x xs = list (Left (x, xs))
let nil = list (right unit)
let case fc fnil (List l) = either fc fnil l
let head (List l) = either first (fun () -> failwith "Empty list") l
let tail (List l) = either second (fun () -> failwith "Empty list") l

let rec map f l =
	case
		(fun p -> unzip cons (both f (map f) p))
		(fun () -> nil)
		l