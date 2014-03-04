//If recursive path doesnt have a function, it just returns the int of a value, if required!
let rec recursivePath inputtree =
  
  let processBody body = 
    (recursivePath body)
    
  let processIf condition body = 
    if ((recursivePath condition) == true)
    then (recursivePath body)
  in
  
  let processIfElse condition body elsebody = 
    if ((recursivePath condition) == true)
    then (recursivePath body) 
    else (recursivePath elsebody) 
  
  in
  /* Could be faulty ?? */
  let processWhile condition body = 
    while ((recursivePath condition)) 
      do (recursivePath body)
    done
    
    

match inputtree with
    LeafBool(x1)                    -> x1
  | Leaf (x1)                       -> x1
  | Variable (name)                 -> (AssignName name)
  | Node1("streamValue", streamName)            ->
  
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
  
  | Node2("globalAssign", x1, x2)
  | Node2("globalAssignExtending", x1, x2)
  
  | Node2("MainwithGlobalVars", x1, x2)
  
  | Node2("if", x1, x2)             -> (processIf x1 x2)
  | Node3("if", x1, x2, x3)         -> (processIfElse x1 x2)
  | Node1("while", x1, x2)          -> (processWhile x1 x2)
  
  | Node2("&&", x1, x2)             -> if (((recursivePath x1) == true) && ((recursivePath x2) == true )) then true else false
  | Node2("||", x1, x2)             -> if (((recursivePath x1) == true) || ((recursivePath x2) == true )) then true else false
  | Node2("==", x1, x2)             -> if ((recursivePath x1) == (recursivePath x2)) then true else false
  | Node2("!=", x1, x2)             -> if ((recursivePath x1) != (recursivePath x2)) then true else false
  | Node2(">", x1, x2)              -> if ((recursivePath x1) > (recursivePath x2)) then true else false
  | Node2(">=", x1, x2)             -> if ((recursivePath x1) >= (recursivePath x2)) then true else false
  | Node2("<", x1, x2)              -> if ((recursivePath x1) < (recursivePath x2)) then true else false
  | Node2("<=", x1, x2)             -> if ((recursivePath x1) <= (recursivePath x2)) then true else false
  | Node1("!", x1)                  -> if ((recursivePath x1) == true) then false else true
  | 
