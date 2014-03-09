open Lexer;;
open Parser;;
open ParseTree;;


module MapOfVariables = Map.Make(String);;
let globalVariables = ref MapOfVariables.empty;;
let localVariables = ref MapOfVariables.empty;; (* We may not need, but I'll just put it here, we can just remove later *)
let streamBinding = ref MapOfVariables.empty;;
let intListRefTable = ref MapOfVariables;;

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
  
  (* Very important, need to check, I think another way, but try this one first *)
  let processGlobalAssign argName argValue = 
    globalVariables := MapOfVariables.add argVName argValue !globalVariables;  
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
    if ((name = "NUMBER_OF_STREAM") || (name "STREAM_LENGTH")) then 
        (Printf.fprintf stderr "The variable '%s' is predefined in the program and cannot be used" name)
    else 
      (* If it's a global variable then save it here  *)
      (VarMap.find name !globalVariables);
      globalVariables := (MapOfVariables.add name (recursivePath value) !globalVariables);
      0
  in
  
  (* Look for and incre the value associated with variable name, then assign new value to the name *)
  let processIncrement name incre =
    let newValue = (processVariable name) + incre in
       (processAssign name (Leaf(newValue))); newValue
  in
  
  (* Look up for the stream in the global binding table, may need to check again *)
  let processStream streamName = 
    try 
      (MapOfVariables.find ("#" (string_of_int streamName)) !streamBinding)
    with Not_found
  in

  let processPrint arg =
    outputStream := (recursivePath arg) :: !outputStream;
    0
    in

match inputTree with
  | Leaf(argInt)                        -> argInt (* ok *)
  | Variable(name)                      -> (processVariable name) 
  | Node1("streamValue", streamName)    -> (Value streamName) 
  
  | Node2("MainwithGlobalVars", arg1, arg2)      -> (recursivePath arg1); (recursivePath arg2)
  | Node2("assign", Variable(name), Leaf(value)) -> (processAssign) (* normal assignment *)
                                        
  (* done *)
  | Node1("bodyOneSentence", arg1)             -> (processBody arg1) (* ok *)
  | Node2("bodyManySentences", arg1, arg2)     -> (processBodyextend arg1 arg2) (* ok *)
  
  | Node2("globalAssign", Variable(arg1), Leaf(arg2))     -> (processGlobalAssign arg1 arg2) (* Important, Need to check *)
  | Node2("globalMultiAssign", arg1, arg2)                -> (recursivePath arg1); (recursivePath arg2)
   
  | Node2("if", arg1, arg2)             -> (processIf arg1 arg2) (* done *)
  | Node3("if", arg1, arg2, arg3)       -> (processIfElse arg1 arg2 arg3) (* done *)
  | Node1("while", arg1, arg2)          -> (processWhile arg1 arg2) (* done *)
  
  | Node1("print", rawvalue)            -> (processPrint rawvalue)
  | Node2("printVarible", variable)     -> (processPrint variable)

  | Node1("++", arg1)                   -> (processIncrement arg1 1) (* done *)
  | Node1("--", arg1)                   -> (processIncrement arg1 (-1)) (* or call processsIncrement with -1 *) (* done *)
  | Node2("+=", Variable(arg1), arg2)   -> (processIncrement arg1 (recursivePath arg2)) (* done *)
  | Node2("-=", Variable(arg1), arg2)   -> (processIncrement arg1 (-(recursivePath arg2))) (* done *)

  | Node1("-", arg1)                    -> -(recursivePath arg1) (* This one is negation of some value *)
  | Node2("+", arg1, arg2)              -> (recursivePath arg1) + (recursivePath arg2)
  | Node2("-", arg1, arg2)              -> (recursivePath arg1) - (recursivePath arg2) (* This one is subtraction *)
  | Node2("*", arg1, arg2)              -> (recursivePath arg1) * (recursivePath arg2)
  | Node2("/", arg1, arg2)              -> (recursivePath arg1) / (recursivePath arg2)
  | Node2("%", arg1, arg2)              -> (recursivePath arg1) mod (recursivePath arg2)
  | Node2("^", arg1, arg2)              -> (recursivePath arg1) ** (recursivePath arg2) (* The ** apply to float *)
  
  | Node2("&&", arg1, arg2)             -> if (((recursivePath arg1) != 0) && ((recursivePath arg2) != 0 )) then 1 else 0
  | Node2("||", arg1, arg2)             -> if (((recursivePath arg1) != 0) || ((recursivePath arg2) != 0 )) then 1 else 0
  | Node2("==", arg1, arg2)             -> if ((recursivePath arg1) == (recursivePath arg2)) then 1 else 0
  | Node2("!=", arg1, arg2)             -> if ((recursivePath arg1) != (recursivePath arg2)) then 1 else 0
  | Node2(">", arg1, arg2)              -> if ((recursivePath arg1) > (recursivePath arg2)) then 1 else 0
  | Node2(">=", arg1, arg2)             -> if ((recursivePath arg1) >= (recursivePath arg2)) then 1 else 0
  | Node2("<", arg1, arg2)              -> if ((recursivePath arg1) < (recursivePath arg2)) then 1 else 0
  | Node2("<=", arg1, arg2)             -> if ((recursivePath arg1) <= (recursivePath arg2)) then 1 else 0
  | Node1("!", arg1)                    -> if ((recursivePath arg1) == 1) then 0 else 1 
   
;;

(* Read line by line and return a list containing refs to each int stream *)
let collect_lists count = match count with
  | 0 -> []
  | n -> ref (List.map int_of_string (Str.split (Str.regexp " ") (read_line ()))) :: collect_lists (x - 1)

(* Store input in a map *)
let storeInput = 
    (* store the first 2 lines that contains information of stream *)
    let streamCount := (read_int());
    globalVars := (MapOfVariables.add "NUMBER_OF_STREAM" streamCount !globalVars);
    let streamLength := (read_int());
    globalVars := (MapOfVariables.add "STREAM_LENGTH" streamLength !globalVars);

    (* Then consume the rest, return inputStream which is a list of ref *)
    inputStream := collect_list count
    
;;

let addElementToMap name value = globalVariables := (MapOfVariables.add name value !globalVariables)

(* Create keys in the map, each key point to element of the head of the stream *)
let rec bindStream n inStream = 
  match inStream with 
    | [] -> ref ()
    | car :: cdr -> addElementToMap ("#" ^ (string_of_int n)) (hd !car); (*  *) 
                    car := (tl !car);
                    let theRest := ref cdr in 
                      addElementToMap (n + 1) theRest

(* Print a list to the specifired format to stdout *)
let rec printList alist = match alist with
  | [] -> ()
  | hd::tl -> (Printf.printf "%d " hd); printList tl

let start =
    storeInput;
    try
        let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
            while true do
                let result = (myParser.main MyLexer.token lexbuf) in (* Need to check this part again *)
                    for i = 0 to !streamLength-1 do
                    streamBinding := MapOfVariables.empty;
                    bindStream 0 inputStream;
                    flush stdout;    
                    recursivePath result;
            done
    with 
        MyLexer.Eof -> 
            print_int (List.length !outputStream);
            print_string "\n";
            printList (List.rev !outputStream);
            print_string "\n";
            exit 0
