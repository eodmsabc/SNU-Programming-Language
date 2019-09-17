(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open Xexp

let removeExn : xexp -> xexp = fun e ->
	let rec recexn xexp =
		match xexp with
		| Num n -> Num n
		| Var x -> Var x
		| Fn (x, e) -> Fn (x, recexn e)
		| App (e1, e2) -> App (recexn e1, recexn e2)
		| If (e, e1, e2) -> If (recexn e, recexn e1, recexn e2)
		| Equal (e1, e2) -> Equal (recexn e1, recexn e2)
		| Raise e ->
		| Handle (e1, n, e2) ->
	Fn recexn xexp
