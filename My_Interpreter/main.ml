open Lexer;;
open Parser;;
open ParseTree;;


module MapOfVariables = Map.Make(String);;
let globalVariables = ref MapOfVariables.empty;;
let localVariables = ref MapOfVariables.empty

(*The below variables are to store information on the stream*)
let streamCount = ref 0;;
let streamLength = ref 0;;


let inputStream = ref [];;
let outputStream = ref [];;


let rec recursivePath inputTree =
  
  let processBody body = 
    (recursivePath body)
  in
  
  let processBodyextend bodyOne bodyTwo = 
    (recursivePath bodyOne); (recursivePath bodyTwo)
  in
  
  let processIf condition body = 
    if ((recursivePath condition) == 1) (* or could be =!0 *)
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
  
  (* Very important, need to check *)
  let processGlobalAssign argName argValue = 
    if (argVName = "begin" or argVName = "openstream") 
    then print_string("Error: Used a illegal variable name");
    else
    globalVariables := MapOfVariables.add argVName (recursivePath argVValue) !globalVariables;  
  in
    
  
  
  (* Seem look for a variable's name and return its associated value *)
  let processVariable name = 
    try
      (MapOfVariables.find name !globalVariables)
      with Not_found -> print_string("Error: This variable does not exist"); exit 0;
  in
  
  (* importtant function used incre/incre_and_assign, decree/decree_and_assing...   *)  
  (* Assign value to a variable name; value could be a tree, that needed to be evaluated!! *)
  let processAssign name value = 
    if ((name = "COUNT") || (name "LENGTH")) then 
        (Printf.fprintf stderr "The variable '%s' is predefined in the program and cannot be used" name);
    else 
      (* If it's a global variable then save it here  *)
      (VarMap.find name !globalVariables);
      globalVariables := (MapOfVariables.add name (recursivePath value) !globalVariables);
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


match inputTree with
  | Leaf(argInt)                         -> argInt (* ok *)
  | Variable(name)                     -> (processVariable name) 
  | Node1("streamValue", streamName)    -> (processStream streamName) 
  
  | Node2("MainwithGlobalVars", arg1, arg2)   -> (recursivePath arg1); (recursivePath arg2)
  | Node2("assign", Variable(name), Leaf(value)) (* normal assignment *)
                                        -> (processAssign)
  
  (* done *)
  | Node1("bodyEnding", arg1)         -> (processBody arg1) (* ok *)
  | Node2("bodyExtend", arg1, arg2)   -> (processBodyextend arg1 arg2) (* ok *)
  
  | Node2("globalAssign", arg1, arg2)           -> (processGlobalAssign arg1 arg2) (* Important, Need to check *)
  | Node2("globalAssignExtending", arg1, arg2)  -> (recursivePath arg1); (recursivePath arg2)
  
  
  
  | Node2("if", arg1, arg2)             -> (processIf arg1 arg2)
  | Node3("if", arg1, arg2, arg3)         -> (processIfElse arg1 arg2 arg3)
  | Node1("while", arg1, arg2)          -> (processWhile arg1 arg2)
  
  | Node1("++", arg1)                   -> (processsIncrement arg1)
  | Node1("--", arg1)                   -> (processDecrement arg1) (* or call processsIncrement with -1 *)
  | Node2("+", arg1, arg2)              -> (recursivePath arg1) + (recursivePath arg2)
  | Node2("-", arg1, arg2)              -> (recursivePath arg1) - (recursivePath arg2)
  | Node2("*", arg1, arg2)              -> (recursivePath arg1) * (recursivePath arg2)
  | Node2("/", arg1, arg2)              -> (recursivePath arg1) / (recursivePath arg2)
  | Node2("%", arg1, arg2)              -> (recursivePath arg1) mod (recursivePath arg2)
  | Node2("^", arg1, arg2)              -> (recursivePath arg1) ** (recursivePath arg2)
  
  | Node2("&&", arg1, arg2)             -> if (((recursivePath arg1) != 0) && ((recursivePath arg2) != 0 )) then 1 else 0
  | Node2("||", arg1, arg2)             -> if (((recursivePath arg1) != 0) || ((recursivePath arg2) != 0 )) then 1 else 0
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
