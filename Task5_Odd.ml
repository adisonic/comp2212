exception List_too_short


let rec truncate n (l:int list)  = match l with
	| [] -> []
	| h::t -> if n = 1 
			then t 
			else truncate (n - 1) t


let rec odd (l:int list) = match l with
	| [] -> []
	| [x] -> [x]
	| h::t::st -> h:: (odd st) 
	 

let rec odd2 (l:int list) = match l with 
	| [] -> []
	| [x] -> [x]
	| h::t -> h:: odd2 (truncate 1 t)






