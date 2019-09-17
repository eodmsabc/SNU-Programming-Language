(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
	| K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
	| K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
	| K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
	| K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]

    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
	| K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
	| K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
	| K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
	| K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
	| K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
	| K.NOT e -> trans e @ [Sm5.NOT]

	| K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
	| K.IF (e, e1, e2) -> trans e @ [Sm5.JTR (trans e1, trans e2)]

	| K.WHILE (e, exp) ->
		trans (
			K.LETV ("#cond", e,
			K.LETF ("#while", "#x",
				K.IF (K.EQUAL (K.VAR "#x", K.FALSE),
					K.UNIT,
					K.SEQ(exp, K.SEQ (K.ASSIGN ("#x", e), K.CALLR ("#while", "#x")))),
			K.CALLR ("#while", "#cond")))
		)
	| K.FOR (i, e1, e2, exp) ->
		trans (
			K.LETV ("#fi", e1,
			K.LETV ("#fe", K.ADD (e2, K.NUM 1),
				K.WHILE(K.LESS (K.VAR "#fi", K.VAR "#fe"),
				K.SEQ (K.ASSIGN (i, K.VAR "#fi"), K.SEQ (exp, K.ASSIGN ("#fi", K.ADD (K.VAR "#fi", K.NUM 1)))))
			))
		)

	| K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.LETV (x, e1, e2) ->
		trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
		trans e2 @ [Sm5.UNBIND; Sm5.POP]
	| K.LETF (f, x, fe, exp) ->
		[Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ trans fe @ [Sm5.UNBIND; Sm5.POP])); Sm5.BIND f] @
		trans exp @ [Sm5.UNBIND; Sm5.POP]
	
	| K.CALLV (f, e) ->
		[Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans e @ [Sm5.MALLOC; Sm5.CALL]
	| K.CALLR (f, x) ->
		[Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ [Sm5.PUSH (Sm5.Id x); Sm5.LOAD; Sm5.PUSH (Sm5.Id x); Sm5.CALL]

    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
	| K.WRITE exp -> trans exp @
		[Sm5.MALLOC; Sm5.BIND "#t"; Sm5.PUSH (Sm5.Id "#t"); Sm5.STORE] @
		[Sm5.PUSH (Sm5.Id "#t"); Sm5.LOAD; Sm5.PUSH(Sm5.Id "#t"); Sm5.LOAD] @
		[Sm5.PUT; Sm5.UNBIND; Sm5.POP]
end
