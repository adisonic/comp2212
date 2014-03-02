exception Uneven_Lists

let rec zip l1 l2 = match l1, l2 with (* or zip pairOfList *)
[] , []  ->  []
| [] , _ -> raise Uneven_Lists
| _  , []  -> raise Uneven_Lists
| h1::t1, h2::t2 -> h1::h2 :: (zip t1 t2)




