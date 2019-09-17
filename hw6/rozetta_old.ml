(*
 * SNU 4190.310 Programming Languages 
 * Homework "Rozetta" Skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open Sm5
open Sonata.Sonata

exception ERROR of string

module Rozetta = struct
	let trans : Sm5.command -> command = fun command ->
		let rec transrec cmd =
			match cmd with
			| [] -> []
			| Sm5.PUSH (Sm5.Val (Sm5.Z i)) :: t -> PUSH (Val (Z i)) :: transrec t
			| Sm5.PUSH (Sm5.Val (Sm5.B b)) :: t -> PUSH (Val (B b)) :: transrec t
			| Sm5.PUSH (Sm5.Val (Sm5.Unit)) :: t -> PUSH (Val Unit) :: transrec t
			| Sm5.PUSH (Sm5.Id s) :: t -> PUSH (Id s) :: transrec t
	
			| Sm5.PUSH (Sm5.Fn (s, c)) :: t -> PUSH (Fn (s, [BIND "#currsf"] @ transrec c @
				[
					PUSH (Id "#currsf");
					PUSH (Val (Z 0));
					MALLOC;
					CALL
				])) :: transrec t
	
			| Sm5.POP :: t -> POP :: transrec t
			| Sm5.STORE :: t -> STORE :: transrec t
			| Sm5.LOAD :: t -> LOAD :: transrec t
			| Sm5.JTR (c1, c2) :: t -> JTR (transrec c1, transrec c2) :: transrec t
			| Sm5.MALLOC :: t -> MALLOC :: transrec t
			| Sm5.BOX i :: t -> (BOX i) :: transrec t
			| Sm5.UNBOX s :: t -> (UNBOX s) :: transrec t
			| Sm5.BIND s :: t -> (BIND s) :: transrec t
			| Sm5.UNBIND :: t -> UNBIND :: transrec t
			| Sm5.GET :: t -> GET :: transrec t
			| Sm5.PUT :: t -> PUT :: transrec t
	
			| Sm5.CALL :: t ->
				[
					PUSH (Fn ("#ta", (transrec t) @
						[
							PUSH (Id "#currsf"); (*LOAD;*)
							PUSH (Val (Z 0));
							MALLOC;
							CALL
						])); (* current state function *)
					BIND "#tempff"; (* bind current state function *)
					BIND "#tl";
					MALLOC; BIND "#tvl"; PUSH (Id "#tvl"); STORE;
					BIND "#tf";
					PUSH (Id "#tempff");
					PUSH (Id "#tf"); UNBIND; POP;
					PUSH (Id "#tvl"); LOAD; UNBIND; POP;
					PUSH (Id "#tl"); UNBIND; POP;
					UNBIND; POP;
					CALL
				]
				(*[
					BIND "#tl";
					MALLOC; BIND "#tvl"; PUSH (Id "#tvl"); STORE;
					BIND "#tf";
					PUSH (Fn ("#ta", transrec t)); (* saving current state function *)
					PUSH (Id "#tf"); UNBIND; POP;
					PUSH (Id "#tvl"); LOAD; UNBIND; POP;
					PUSH (Id "#tl"); UNBIND; POP;
					CALL
				]*)

	
			| Sm5.ADD :: t -> ADD :: transrec t
			| Sm5.SUB :: t -> SUB :: transrec t
			| Sm5.MUL :: t -> MUL :: transrec t
			| Sm5.DIV :: t -> DIV :: transrec t
			| Sm5.EQ :: t -> EQ :: transrec t
			| Sm5.LESS :: t -> LESS :: transrec t
			| Sm5.NOT :: t -> NOT :: transrec t
			| _ -> raise (ERROR "Unimplemented")
		in
		(*[PUSH (Fn ("#ta", [])); MALLOC; BIND "#currsf"; PUSH (Id "#currsf"); STORE]*)
		[PUSH (Fn ("#ta", [])); BIND "#currsf"]
		@ transrec command
end
