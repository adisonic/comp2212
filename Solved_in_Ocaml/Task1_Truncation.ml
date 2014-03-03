exception Stream_too_short

let rec truncate n (l:int list)  = match l with
	| [] -> raise Stream_too_short
	| h::t -> if n = 1 
			then t 
			else truncate (n - 1) t
