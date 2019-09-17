let rec delete item lst =
	match lst with
	| [] -> lst
	| h :: t ->
		if (h=item) then (
			delete item lst
		)
		else (
			h :: (delete item lst)
		)


let rec listcompare lst1 lst2 =
try
	if ((List.length lst1) = (List.length lst2)) then (
		match lst1 with
		| [] -> true
		| h :: t -> ((List.mem h lst2) && (listcompare t (delete h lst2)))
	)
	else (
		false
	)
with Not_found -> false
