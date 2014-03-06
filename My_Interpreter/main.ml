open Lexer;;
open Parser;;
open ParseTree;;


module MapOfVariables = Map.Make(String);;
let globalVariables = ref MapOfVariables.empty;;


(*The below variables are to store information on the stream*)
let streamCount = ref 0;;
let streamLength = ref 0;;


let inputStream = ref [];;
let outputStream = ref [];;


let rec recursivePath inputTree =
  
  let processVariable argVName = 
    try
      (MapOfVariables.find argVName !globalVariables)
      with Not_found -> print_string("Error: This variable does not exist"); exit 0;
      
  in
  
  let assignName argVName argVValue = 
    if (argVName = "begin" or argVName = "openstream") 
    then print_string("Error: Used a illegal variable name");
    else
    globalVariables := MapOfVariables.add argVName (recursivePath argVValue) !globalVariables;  
  in
  
  let processBody body = 
    (recursivePath body)
  in
  
  let processBodyextend bodyOne bodyTwo = 
    (recursivePath bodyOne);
    (recursivePath bodyTwo);
    
  let processIf condition body = 
    if ((recursivePath condition) == 1)
    then (recursivePath body)
    else 0
  in
  
  let processIfElse condition body elsebody = 
    if ((recursivePath condition) == 1)
    then (recursivePath body) 
    else (recursivePath elsebody) 
  
  in
  (* Could be faulty ?? *)
  let processWhile condition body = 
    while ((recursivePath condition) == 1) 
      do (recursivePath body)
    done;
    0
    
  in
  let processIncrement arg1 =
    print_string("increment shit");
  in
  
  let processDecrement arg1 = 
    print_string("decrement shit");
    
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
  
  | Node1("++", arg1)                   -> (processsIncrement arg1)
  | Node1("--", arg1)                   -> (processDecrement arg1)
  | Node2("+", arg1, arg2)              -> (recursivePath arg1) + (recursivePath arg2)
  | Node2("-", arg1, arg2)              -> (recursivePath arg1) - (recursivePath arg2)
  | Node2("*", arg1, arg2)              -> (recursivePath arg1) * (recursivePath arg2)
  | Node2("/", arg1, arg2)              -> (recursivePath arg1) / (recursivePath arg2)
  | Node2("%", arg1, arg2)              -> (recursivePath arg1) mod (recursivePath arg2)
  | Node2("^", arg1, arg2)              -> (recursivePath arg1) ** (recursivePath arg2)
  
  | Node1("bodyEnding", arg1)         -> (recursivePath arg1)
  | Node2("bodyExtend", arg1, arg2)   -> (recursivePath arg1); (recursivePath arg2)
  
  | Node2("globalAssign", Variable(arg1), arg2)           -> (assignName arg1 arg2)
  | Node2("globalAssignExtending", arg1, arg2)  -> (recursivePath arg1); (recursivePath arg2)
  
  | Node2("MainwithGlobalVars", arg1, arg2)   -> (recursivePath arg1); (recursivePath arg2)
  
  | Node2("if", arg1, arg2)             -> (processIf arg1 arg2)
  | Node3("if", arg1, arg2, arg3)         -> (processIfElse arg1 arg2 arg3)
  | Node1("while", arg1, arg2)          -> (processWhile arg1 arg2)
  
  | Node2("&&", arg1, arg2)             -> if (((recursivePath arg1) == 1) && ((recursivePath arg2) == 1 )) then 1 else 0
  | Node2("||", arg1, arg2)             -> if (((recursivePath arg1) == 1) || ((recursivePath arg2) == 1 )) then 1 else 0
  | Node2("==", arg1, arg2)             -> if ((recursivePath arg1) == (recursivePath arg2)) then 1 else 0
  | Node2("!=", arg1, arg2)             -> if ((recursivePath arg1) != (recursivePath arg2)) then 1 else 0
  | Node2(">", arg1, arg2)              -> if ((recursivePath arg1) > (recursivePath arg2)) then 1 else 0
  | Node2(">=", arg1, arg2)             -> if ((recursivePath arg1) >= (recursivePath arg2)) then 1 else 0
  | Node2("<", arg1, arg2)              -> if ((recursivePath arg1) < (recursivePath arg2)) then 1 else 0
  | Node2("<=", arg1, arg2)             -> if ((recursivePath arg1) <= (recursivePath arg2)) then 1 else 0
  | Node1("!", arg1)                  -> if ((recursivePath arg1) == 1) then 0 else 1 
   
;;

(* Store input in a map *)
let storeInput = 
    let count := (read_int());
    globalVars := (VarMap.add("Count") count !globalVars);
    let length := (read_int());
    globalVars := (VarMap.add("Length") length !globalVars);

    let bind label value 

    while (!count != 0) do 
        (* extract each line of int *)
        let anIntList = (List.map int_of_string (Str.split (Str.regexp " ") (read_line ()))); 
        (* store them in the globalVar map *)
        globalVar := (VarMap.add "$" ^ )
;;



let start =
    inputGetter;
    try
        let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
            while true do
                let result = (myParser.main MyLexer.token lexbuf) in
                        flush stdout;    
                        recursivePath result;
            done;
    with 
        MyLexer.Eof -> 
            print_int (List.length !outputStream);
            print_string "\n";
            printList (List.rev !outputStream);
            (exit 0)
