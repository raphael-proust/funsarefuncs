open Fn
open Fc

let print s = Printf.printf "%s\n" s
let fail _ = failwith "fail"
module Print (V: VAL with type t = string) : UNIT =
	struct
		let () = print V.v
		include Unit
	end
module Fail (V: VAL) : VAL =
	struct
		let () = fail ()
		include Unit
	end
module STRINGEQ = struct type t = string end

let wrap s =
	(module struct type t = string let v = s end : VAL with type t = string)

let v = left (pair (pair "foo" "bar") (right "blah"))
let _ =
	either
		(both
			(both print print)
			(either
				fail
				print
			)
		)
		fail
		v


module Foo = struct type t = string let v = "foo" end
module Bar = struct type t = string let v = "bar" end
module Blah = struct type t = string let v = "blah" end
module FooBar = Pair (Foo) (Bar)
module V : PROD with type A.t = STRINGEQ.t and type B.t = STRINGEQ.t = (Pair (Foo) (Bar))
module VV = (Right (STRINGEQ) (Blah))

module M = Both (Unit) (Unit) (V) (Print) (Print)