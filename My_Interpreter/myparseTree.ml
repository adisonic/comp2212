(*Parse tree will go in here*)

type parseTree = 
	| Leaf of (int) (*Boolean*)
	| Node1 of (id * parseTree)
	| Node2 of (id * parseTree * parseTree)
	| Node3 of (id * parseTree * parseTree * parseTree)



