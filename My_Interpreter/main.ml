(*If recursive path doesnt have a function, it just returns the int of a value, if required!
*)
open mylexer;
open myparser;
open myparseTree;

module MapOfVariables = map.make(String);;
let GlobalVariables = ref MapOfVariables.empty;;

(*The below variables are to store information on the stream*)
(*let streamCount = ref 0;
let streamLength = ref 0;
*)
let inputStream = ref [];
let outputStream = ref [];

let rec recursivePath inputTree =
  
  let processVariable argVName = 
    try
      (MapOfVariables.find argVName !GlobalVariables)
      with Not_found -> print_string("Error: This variable does not exist"); exit 0;
      
  in
  
  let assignName argVName argVValue = 
    GlobalVariables := MapOfVariables.add argVName (recursivePath argVValue) !GlobalVariables;  
  in
  
  let processBody body = 
    (recursivePath body)
  in
  
  let processBodyextend bodyOne bodyTwo = 
    (recursivePath bodyOne);
    (recursivePath bodyTwo);
    
  let processIf condition body = 
    if ((recursivePath condition) == true)
    then (recursivePath body)
    else false
  in
  
  let processIfElse condition body elsebody = 
    if ((recursivePath condition) == true)
    then (recursivePath body) 
    else (recursivePath elsebody) 
  
  in
  (* Could be faulty ?? *)
  let processWhile condition body = 
    while ((recursivePath condition) == true) 
      do (recursivePath body)
    done;
    false
    
  in
  let print arg =
    outputStream := (recursivePath arg) :: !outputStream;
    0
    in

match inputtree with
    LeafBool(argBool)                   -> argBool
  | Leaf (argInt)                         -> argInt
  | Variable (name)                     -> (processVariable name)
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
  
  | Node2("globalAssign", arg1, arg2)           -> (assignName arg1 arg2)
  | Node2("globalAssignExtending", arg1, arg2)  -> (recursivePath arg1); (recursivePath arg2)
  
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
;;

let inputGetter =  
  let StreamCount = (read_int()) in
  globalVariables
  let StreamLength = (read_int()) in
  
  

;;

let start =
    inputGetter;
    try
        let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
            while true do
                let result = (myParser.main Lexer.token lexbuf) in
                        flush stdout;    
                        recursivePath result;
            done;
    with 
        Lexer.Eof -> 
            print_int (List.length !outputStream);
            print_string "\n";
            printList (List.rev !outputStream);
            (exit 0)
