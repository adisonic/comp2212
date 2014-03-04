(*If recursive path doesnt have a function, it just returns the int of a value, if required!
*)
open mylexer;
open myparser;
open myparseTree;

module MapOfVariables = map.make(String);;
let GlobalVariables = ref MapOfVariables.empty;;

(*The below variables are to store information on the stream*)
let streamCount = ref 0;
let streamLength = ref 0;


let rec recursivePath inputtree =
  
  let processBody body = 
    (recursivePath body)
  in
  
  let processBody extend bodyOne bodyTwo = 
    (recursivePath bodyOne);
    (recursivePath bodyTwo);
    
  let processIf condition body = 
    if ((recursivePath condition) == true)
    then (recursivePath body)
  in
  
  let processIfElse condition body elsebody = 
    if ((recursivePath condition) == true)
    then (recursivePath body) 
    else (recursivePath elsebody) 
  
  in
  (* Could be faulty ?? *)
  let processWhile condition body = 
    while ((recursivePath condition)) 
      do (recursivePath body)
    done
    
    

match inputtree with
    LeafBool(argBool)                   -> argBool
  | Leaf (arg1)                         -> arg1
  | Variable (name)                     -> (AssignName name)
  | Node1("streamValue", streamName)    -> (processStream streamName) 
  
    Node1("++", arg1)
  | Node1("--", arg1)
  |
  | Node2("+", arg1, arg2)              -> (recursivePath arg1) + (recursivePath arg2)
  | Node2("-", arg1, arg2)              -> (recursivePath arg1) - (recursivePath arg2)
  | Node2("*", arg1, arg2)              -> (recursivePath arg1) * (recursivePath arg2)
  | Node2("/", arg1, arg2)              -> (recursivePath arg1) / (recursivePath arg2)
  | Node2("%", arg1, arg2)              -> (recursivePath arg1) mod (recursivePath arg2)
  | Node2("^", arg1, arg2)              -> (recursivePath arg1) ** (recursivePath arg2)
  
  | Node1("bodyEnding", arg1)         -> (recursivePath arg1)
  | Node2("bodyExtend", arg1, arg2)   -> (recursivePath arg1); (recursivePath arg2)
  
  | Node2("globalAssign", arg1, arg2)
  | Node2("globalAssignExtending", arg1, arg2)
  
  | Node2("MainwithGlobalVars", arg1, arg2)   -> (recursivePath arg1); (recursivePath arg2)
  
  | Node2("if", arg1, arg2)             -> (processIf arg1 arg2)
  | Node3("if", arg1, arg2, arg3)         -> (processIfElse arg1 arg2 arg3)
  | Node1("while", arg1, arg2)          -> (processWhile arg1 arg2)
  
  | Node2("&&", arg1, arg2)             -> if (((recursivePath arg1) == true) && ((recursivePath arg2) == true )) then true else false
  | Node2("||", arg1, arg2)             -> if (((recursivePath arg1) == true) || ((recursivePath arg2) == true )) then true else false
  | Node2("==", arg1, arg2)             -> if ((recursivePath arg1) == (recursivePath arg2)) then true else false
  | Node2("!=", arg1, arg2)             -> if ((recursivePath arg1) != (recursivePath arg2)) then true else false
  | Node2(">", arg1, arg2)              -> if ((recursivePath arg1) > (recursivePath arg2)) then true else false
  | Node2(">=", arg1, arg2)             -> if ((recursivePath arg1) >= (recursivePath arg2)) then true else false
  | Node2("<", arg1, arg2)              -> if ((recursivePath arg1) < (recursivePath arg2)) then true else false
  | Node2("<=", arg1, arg2)             -> if ((recursivePath arg1) <= (recursivePath arg2)) then true else false
  |(* Node1("!", arg1)                  -> if ((recursivePath arg1) == true) then false else true  *)
  | 
