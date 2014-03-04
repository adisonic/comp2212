(*Parse tree will go in here*)

type parseTree = 
	| Leaf of (int) 
	| LeafBool of (bool)
	| Name of (string)
	| Node1 of (string * parseTree)
	| Node2 of (string * parseTree * parseTree)
	| Node3 of (string * parseTree * parseTree * parseTree)



