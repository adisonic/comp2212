
//If recursive path doesnt have a function, it just returns the int of a value, if required!
let rec recursivePath inputtree {
  if (inputtree

match inputtree with
    Node1("++", $1)
  | Node1("--", $1)
  |
  | Node2("+", x1, x3)              -> (recursivePath x1) + (recursivePath x3)
  | Node2("-", x1, x3)              -> (recursivePath x1) - (recursivePath x3)
  | Node2("*", x1, x3)              -> (recursivePath x1) * (recursivePath x3)
  | Node2("/", x1, x3)              -> (recursivePath x1) / (recursivePath x3)
  | Node2("%", x1, x3)              -> (recursivePath x1) mod (recursivePath x3)
  | Node2("^", x1, x3)              -> (recursivePath x1) ** (recursivePath x3)
  
  | Node1("bodyEnding", $1)
  | Node2("bodyExtend", $1, $2)
  
  | Node
  
  | Node2("if", $2, $4)
  | Node3("if, $2, $4, $8)
  
  | Node2("&&", $1, $3)
  | Node2("||", $1, $3)
  | Node2("==", $1, $3)
  | Node2("!=", $1, $3)
  
