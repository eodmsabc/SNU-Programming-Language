type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure | Branch of map * map | Guide of string * map

type gNode = M of int | T of string | S
type vset = G of int * (gNode list * int * int)

exception IMPOSSIBLE
exception NF of int

let getReady map =
	let rec nodenum m =
		match m with
		| End t -> 1
		| Branch (m1, m2) -> 1 + (nodenum m1) + (nodenum m2)
		| Guide (t, m) -> 1 + (nodenum m)
	in
	let maxnum = nodenum map in

	let rec vset_init m n (lst, ind) =
		match m with
		| End t ->
			(match t with
			| StarBox ->
				if (List.mem_assoc S ind) then (lst, ind)
				else ((n, ([S], 0-(2*n), 0-(2*n)-1)) :: lst, (S, n) :: ind)
			| NameBox name ->
				if (List.mem_assoc (T name) ind) then (lst, ind)
				else ((n, ([T name], 0-(2*n), 0-(2*n)-1)) :: lst, (T name, n) :: ind)
			)
		| Branch (m1, m2) ->
			let newin = ((n, ([M n], 0-(2*n), 0-(2*n)-1)) :: lst, (M n, n) :: ind)  in
			vset_init m2 (n + (nodenum m1) + 1) (vset_init m1 (n+1) newin)
		| Guide (s, m') ->
			let newin = ((n, ([M n], 0-(2*n), 0-(2*n)-1)) :: lst, (M n, n) :: ind) in
			vset_init m' (n+1) newin
	in
	let init = vset_init map 1 ([], []) in
	
	let getID m n =
		match m with
		| End (StarBox) -> S
		| End (NameBox name) -> T name
		| _ -> M n
	in

	let rec mergeList l1 l2 =
		match l2 with
		| [] -> l1
		| h :: t->
			if (List.mem h l1) then mergeList l1 t
			else mergeList (h :: l1) t
	in

	let rec reallocg gra n1 n2 =
		match gra with
		| [] -> gra
		| h :: t ->
			let (k, (l, a, b)) = h in
			if a = n1 then (
				if b = n1 then (k, (l, n2, n2)) :: (reallocg t n1 n2)
				else (k, (l, n2, b)) :: (reallocg t n1 n2)
			)
			else (
				if b = n1 then (k, (l, a, n2)) :: (reallocg t n1 n2)
				else h :: (reallocg t n1 n2)
			)
	in
	let rec realloci ind lst n2 =
		match ind with
		| [] -> ind
		| h :: t ->
			let (a, b) = h in
			if (List.mem a lst) then (a, n2) :: (realloci t lst n2)
			else h :: (realloci t lst n2)
	in

	let rec merge (gra, ind) n1 n2 oc =
		if oc > (10 * maxnum) then raise IMPOSSIBLE
		else (
			if n1<0 then (reallocg gra n1 n2, ind)
			else if (n1>=0 && n2<0) then (reallocg gra n2 n1, ind)
			else (
				let (b1, c1, d1) = List.assoc n1 gra in
				let (b2, c2, d2) = List.assoc n2 gra in
				let gra' = List.remove_assoc n1 (List.remove_assoc n2 gra) in
				let gra'' = (n2, ((mergeList b1 b2), c2, d2)) :: gra' in
				let ind' = realloci ind b1 n2 in
				let grap = reallocg gra'' n1 n2 in
				merge (merge (grap, ind') c1 c2 (oc+1)) d1 d2 (oc+1)
			)
		)
	in

	let fID (gra, ind) node =
		List.assoc node ind
	in

	let rec setGraph m s n =
	try
		let (gra, ind) = s in
		match m with
		| End t -> (gra, ind)
		| Branch (m1, m2) ->
			let n1 = nodenum m1 in
			let (a, b, c) = List.assoc (fID s (getID m1 (n+1))) gra in
			let set = merge s (fID s (getID m2 (n+n1+1))) b 1 in
			let set' = merge set (fID set (getID m n)) c 1 in
			setGraph m2 (setGraph m1 set' (n+1)) (n+n1+1)
		| Guide (str, m') ->
			let (a, b, c) = List.assoc (fID s (getID m n)) gra in
			let set = merge s (fID s (T str)) b 1 in
			let set' = merge set (fID set (getID m' (n+1))) c 1 in
			setGraph m' set' (n+1)
	with Not_found -> raise IMPOSSIBLE
	in
	let fset = setGraph map init 1 in
	fset
	(*
	let (fg, fi) = fset in

	let rec groupList gra lst =
		match gra with
		| [] -> lst
		| h :: t ->
			let (k, _) = h in
			k :: (groupList t lst)
	in
	let gList = groupList fg [] in
	
	let rec detKey n gra visited =
		if n<0 then raise IMPOSSIBLE
		else if (List.mem n visited) then raise IMPOSSIBLE
		else (
			let (l, e1, e2) = List.assoc n gra in
			if e1<0 then Bar
			else Node (detKey e1 gra (n::visited), detKey e2 gra (n::visited))
		)
	in

	let rec getKey gra gl klst =
		match gl with
		| [] -> klst
		| h :: t ->
			let key_h = detKey h gra [] in
			let (l, _, _) = List.assoc h gra in
			if (List.mem S l) then (
				if (key_h = Bar) then (h, key_h) :: (getKey gra t klst)
				else raise IMPOSSIBLE
			)
			else (h, key_h) :: (getKey gra t klst)
	in

	let fklst = getKey fg gList [] in

	let rec makeKeyList fkl ind lst =
		match ind with
		| [] -> lst
		| h :: t ->
			let (k, v) = h in
			(match k with
				| M n -> makeKeyList fkl t lst
				| _ ->
					let key_k = List.assoc v fkl in
					if (List.mem key_k lst) then makeKeyList fkl t lst
					else makeKeyList fkl t (key_k :: lst)
			)
	in
	makeKeyList fklst fi []*)

let t1 = Guide ("x", End(NameBox "x"))
let t2 = Guide ("y", End(NameBox "y"))
let m = Branch (t1, t2)
let ex = Branch(End(NameBox "x"), Branch(End(NameBox "y"), End(NameBox "x")))
