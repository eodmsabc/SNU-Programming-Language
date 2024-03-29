(*
 * SNU 4190.310 Programming Languages 2015 Fall
 *  K- Interpreter Skeleton Code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

(* Location Signature *)
module type LOC =
sig
	type t
	val base : t
	val equal : t -> t -> bool
	val diff : t -> t -> int
	val increase : t -> int -> t
end

module Loc : LOC =
struct
	type t = Location of int
	let base = Location(0)
	let equal (Location(a)) (Location(b)) = (a = b)
	let diff (Location(a)) (Location(b)) = a - b
	let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
	type 'a t
	exception Not_allocated
	exception Not_initialized
	val empty : 'a t (* get empty memory *)
	val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
	val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
	val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
	type ('a, 'b) t
	exception Not_bound
	val empty : ('a, 'b) t (* get empty environment *)
	val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
	val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
	exception Not_allocated
	exception Not_initialized
	type 'a content = V of 'a | U
	type 'a t = M of Loc.t * 'a content list
	let empty = M (Loc.base,[])

	let rec replace_nth = fun l n c -> 
		match l with
		| h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
		| [] -> raise Not_allocated

	let load (M (boundary,storage)) loc =
		match (List.nth storage ((Loc.diff boundary loc) - 1)) with
		| V v -> v
		| U -> raise Not_initialized

	let store (M (boundary,storage)) loc content =
		M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

	let alloc (M (boundary,storage)) = 
		(boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
	exception Not_bound
	type ('a, 'b) t = E of ('a -> 'b)
	let empty = E (fun x -> raise Not_bound)
	let lookup (E (env)) id = env id
	let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
	exception Error of string
	type id = string
	type exp =
	| NUM of int | TRUE | FALSE | UNIT
	| VAR of id
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| EQUAL of exp * exp
	| LESS of exp * exp
	| NOT of exp
	| SEQ of exp * exp            (* sequence *)
	| IF of exp * exp * exp       (* if-then-else *)
	| WHILE of exp * exp          (* while loop *)
	| LETV of id * exp * exp      (* variable binding *)
	| LETF of id * id list * exp * exp (* procedure binding *)
	| CALLV of id * exp list      (* call by value *)
	| CALLR of id * id list       (* call by referenece *)
	| RECORD of (id * exp) list   (* record construction *)
	| FIELD of exp * id           (* access record field *)
	| ASSIGN of id * exp          (* assgin to variable *)
	| ASSIGNF of exp * id * exp   (* assign to record field *)
	| READ of id
	| WRITE of exp

	type program = exp
	type memory
	type env
	type value =
	| Num of int
	| Bool of bool
	| Unit
	| Record of (id -> Loc.t)
	val emptyMemory : memory
	val emptyEnv : env
	val run : memory * env * program -> value
end

module K : KMINUS =
struct
	exception Error of string

	type id = string
	type exp =
	| NUM of int | TRUE | FALSE | UNIT
	| VAR of id
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| EQUAL of exp * exp
	| LESS of exp * exp
	| NOT of exp
	| SEQ of exp * exp            (* sequence *)
	| IF of exp * exp * exp       (* if-then-else *)
	| WHILE of exp * exp          (* while loop *)
	| LETV of id * exp * exp      (* variable binding *)
	| LETF of id * id list * exp * exp (* procedure binding *)
	| CALLV of id * exp list      (* call by value *)
	| CALLR of id * id list       (* call by referenece *)
	| RECORD of (id * exp) list   (* record construction *)
	| FIELD of exp * id           (* access record field *)
	| ASSIGN of id * exp          (* assgin to variable *)
	| ASSIGNF of exp * id * exp   (* assign to record field *)
	| READ of id
	| WRITE of exp
	
	type program = exp
	type value =
	| Num of int
	| Bool of bool
	| Unit
	| Record of (id -> Loc.t)

	type memory = value Mem.t
	type env = (id, env_entry) Env.t
	and  env_entry = Addr of Loc.t | Proc of id list * exp * env

	let emptyMemory = Mem.empty
	let emptyEnv = Env.empty

	let value_int v =
		match v with
		| Num n -> n
		| _ -> raise (Error "TypeError : not int")

	let value_bool v =
		match v with
		| Bool b -> b
		| _ -> raise (Error "TypeError : not bool")

	let value_unit v =
		match v with
		| Unit -> ()
		| _ -> raise (Error "TypeError : not unit")

	let value_record v =
		match v with
		| Record r -> r
		| _ -> raise (Error "TypeError : not record")

	let lookup_env_loc e x =
		try
			(match Env.lookup e x with
			| Addr l -> l
			| Proc _ -> raise (Error "TypeError : not addr")) 
		with Env.Not_bound -> raise (Error "Unbound")

	let lookup_env_proc e f =
		try
			(match Env.lookup e f with
			| Addr _ -> raise (Error "TypeError : not proc") 
			| Proc (id, exp, env) -> (id, exp, env))
		with Env.Not_bound -> raise (Error "Unbound")

	let rec eval mem env e =
		match e with
		| NUM v ->
			(Num v, mem)
		| TRUE ->
			(Bool true, mem)
		| FALSE ->
			(Bool false, mem)
		| UNIT ->
			(Unit, mem)
		| VAR x ->
			let l = (lookup_env_loc env x) in
			(Mem.load mem l, mem)
		| ADD (e1, e2) ->
			let (a, mem') = (eval mem env e1) in
			let (b, mem'') = (eval mem' env e2) in
			(Num((value_int a) + (value_int b)), mem'')
		| SUB (e1, e2) ->
			let (a, mem') = (eval mem env e1) in
			let (b, mem'') = (eval mem' env e2) in
			(Num((value_int a) - (value_int b)), mem'')
		| MUL (e1, e2) ->
			let (a, mem') = (eval mem env e1) in
			let (b, mem'') = (eval mem' env e2) in
			(Num((value_int a) * (value_int b)), mem'')
		| DIV (e1, e2) ->
			let (a, mem') = (eval mem env e1) in
			let (b, mem'') = (eval mem' env e2) in
			(Num((value_int a) / (value_int b)), mem'')
		| EQUAL (e1, e2) ->
			let (a, mem') = (eval mem env e1) in
			let (b, mem'') = (eval mem' env e2) in
			(match a, b with
			| Num x, Num y -> (Bool (x = y), mem'')
			| Bool x, Bool y -> (Bool (x = y), mem'')
			| Unit, Unit -> (Bool true, mem'')
			| _, _ -> (Bool false, mem''))
		| LESS (e1, e2) ->
			let (a, mem') = (eval mem env e1) in
			let (b, mem'') = (eval mem' env e2) in
			(Bool ((value_int a) < (value_int b)), mem'')
		| NOT e ->
			let (a, mem') = (eval mem env e) in
			(Bool (not (value_bool a)), mem')
		| SEQ (e1, e2) ->
			let (_, mem') = (eval mem env e1) in
			eval mem' env e2
		| IF (e, e1, e2) ->
			let (a, mem') = (eval mem env e) in
			if (value_bool a) then eval mem' env e1
			else eval mem' env e2
		| WHILE (e1, e2) ->
			let (a, mem') = (eval mem env e1) in
			if (value_bool a) then
				let (b, mem'') = (eval mem' env e2) in
				eval mem'' env (WHILE (e1, e2))
			else (Unit, mem')
		| LETV (x, e1, e2) ->
			let (v, mem') = eval mem env e1 in
			let (l, mem'') = Mem.alloc mem' in
			eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
		| LETF (f, plist, e1, e2) ->
			eval mem (Env.bind env f (Proc (plist, e1, env))) e2
		| CALLV (f, explist) ->
			let (plist, exp, env') = lookup_env_proc env f in
			let rec vlink pl el env1 env2 m =
				(match pl, el with
				| h1::t1, h2::t2 ->
					let (ne, nm) = vlink t1 t2 env1 env2 m in
					let (v, nnm) = eval nm env h2 in
					let (l, nnnm) = Mem.alloc nnm in
					let nnnnm = Mem.store nnnm l v in
					(Env.bind ne h1 (Addr l), nnnnm)
				| h::t, [] -> raise(Error "InvalidArg")
				| [], h::t -> raise(Error "InvalidArg")
				| [], [] -> (env1, m)
				) in
			let (envir, memory) = vlink (List.rev plist) (List.rev explist) env' env mem in
			eval memory (Env.bind envir f (Proc (plist, exp, env'))) exp
		| CALLR (f, idlist) ->
			let (plist, exp, env') = lookup_env_proc env f in
			let rec rlink pl il env1 env2 =
				(match pl, il with
				| h1::t1, h2::t2 -> Env.bind (rlink t1 t2 env1 env2) h1 (Addr (lookup_env_loc env2 h2))
				| h::t, [] -> raise (Error "InvalidArg")
				| [], h::t -> raise (Error "InvalidArg")
				| [], [] -> env1
				) in
			let ne = rlink (List.rev plist) (List.rev idlist) env' env in
			eval mem (Env.bind ne f (Proc (plist, exp, env'))) exp
		| RECORD rlist ->
			if rlist = [] then (Unit, mem)
			else
			let rec recbind members m =
				(match members with
				| h::t ->
					let (pf, m1) = recbind t m in
					let (i, e) = h in
					let (v, m2) = eval m1 env e in
					let (l, m3) = Mem.alloc m2 in
					((fun x -> if x = i then l else pf x), (Mem.store m3 l v))
				| [] -> ((fun x -> raise (Error "Unbound")), m)
				) in
			let (recf, mem') = recbind (List.rev rlist) mem in
			(Record recf, mem')
		| FIELD (e, x) ->
			let (v, mem') = eval mem env e in
			let value = Mem.load mem' ((value_record v) x) in
			(value, mem')
		| ASSIGN (x, e) ->
			let (v, mem') = eval mem env e in
			let l = lookup_env_loc env x in
			(v, Mem.store mem' l v)
		| ASSIGNF (e1, x, e2) ->
			let (r, mem') = eval mem env e1 in
			let (v, mem'') = eval mem' env e2 in
			let l = (value_record r) x in
			(v, Mem.store mem'' l v)
		| READ x ->
			let v = Num (read_int()) in
			let l = lookup_env_loc env x in
			(v, Mem.store mem l v)
		| WRITE e ->
			let (v, mem') = eval mem env e in
			let n = value_int v in
			let _ = print_endline (string_of_int n) in
			(v, mem')

	let run (mem, env, pgm) = 
		let (v, _ ) = eval mem env pgm in
		v
end
