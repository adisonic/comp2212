(* Take 2 sequences a1 a2 a3... and b1 b2 b3... 
and produce the sequence a1 - 2*b1 a2 - 2*b2 a3 - 2*b3 ... *) 

exception Uneven_Lists

let rec combine l1 l2 = match l1, l2 with 
	 [] , []  ->  []
| [] , _ -> raise Uneven_Lists
| _  , []  -> raise Uneven_Lists
| h1::t1, h2::t2 -> h1 - 2*h2 :: (combine t1 t2)

