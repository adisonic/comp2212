open ParseTree;;
open List;;
open Lexer;;
                                                                       
module VariableBinding = Map.Make(String);;
let streamBinding = ref VariableBinding.empty;; (* Store at most one element from each stream, get cleared whenever restriving *)
let globalVars =  ref VariableBinding.empty;; (* Always available *) 

(* Stream information *)
let outputStream = ref [];;
let inputStream = ref [];;

let numberOfStreams = ref 0;;
let streamLength = ref 0;;

let entryPointSet = ref false;;
let entryPoint = ref (Leaf(0));;


let rec recursivePath inputTree = 

    let processBody arg = 
        (recursivePath arg)
    in

    let processBodyMany bodyOne bodyTwo = 
        (recursivePath bodyOne);
        (recursivePath bodyTwo)
    in

    let processPrint arg =
        (* let v = (recursivePath arg) in *)
        outputStream := (recursivePath arg) :: !outputStream;
        0
    in

    let processIf cond body = 
        if (recursivePath cond)!=0 then (recursivePath body) else 0
    in

    let processIfElse cond consequence alternate = 
        if (recursivePath cond)!=0 then (recursivePath consequence) else (recursivePath alternate)
    in

    (* Look up for the value associated with variable name *)
    let processVariable name = 
        try
            (VariableBinding.find name !streamBinding)
        with Not_found -> try 
            (VariableBinding.find name !globalVars)              
        with Not_found -> (print_string "Variable '%s' does not exist"); (exit 0)
    in

    let processAssign name value = 
            try
                (* If it's a global variable save it here *)
                (VariableBinding.find name !globalVars);
                globalVars := (VariableBinding.add name (recursivePath value) !globalVars);
                0  
            with Not_found ->
                (* If not a global var then save it as streambinding which is local *)
                streamBinding := (VariableBinding.add name (recursivePath value) !streamBinding);
                0
    in

    let processIncrement name n = 
        let newValue = (processVariable name) + n in
            (processAssign name (Leaf(newValue))); newValue
    in

    let processTimes name n = 
        let newValue = (processVariable name) * n in
            (processAssign name (Leaf(newValue))); newValue
    in

    let processDivide name n = 
        let newValue = (processVariable name) / n in
            (processAssign name (Leaf(newValue))); newValue
    in

    let processGlobalAsign name value = 
        globalVars := VariableBinding.add name value !globalVars;
        0
    in

    let processWhile cond body = 
        while ((recursivePath cond)!=0) do
            (recursivePath body);
        done;
        0
    in

    (* look for the stream value in binding map *)
    let processStreamValue streamName = 
            (VariableBinding.find ("#" ^ (string_of_int streamName)) !streamBinding)
       
    in

    let updateEntryPoint node = 
        (entryPoint := node); 
        (entryPointSet := true); 
        0
    in

    (* pattern mactching the input *) 
    match inputTree with 
        | Variable( arg1 )                     -> (processVariable arg1)
        | Leaf( arg1 )                         -> arg1
        | Node1("value", arg1)                 -> (recursivePath arg1)
        | Node1("streamValue", arg1)           -> (processStreamValue (recursivePath arg1))
        | Node1("bodyOneSentence", arg1)       -> if (!entryPointSet == false) then (updateEntryPoint inputTree) else 0; (processBody arg1) 
        | Node2( "bodyManySentences", arg1, arg2)         
                                               -> if (!entryPointSet == false) then (updateEntryPoint inputTree) else 0; (processBodyMany arg1 arg2)
        | Node1("print", arg1)                 -> (processPrint arg1)
        | Node1("printVariable", arg1)         -> (processPrint arg1)
        
        | Node1("!", arg1)                     -> if ((recursivePath arg1) == 0) then 1 else 0
        | Node1("++", Variable( arg1))         -> (processIncrement arg1 1)
        | Node1("--", Variable( arg1 ))        -> (processIncrement arg1 (-1))
        | Node2( "assign", Variable( arg1 ), arg2)    
                                               -> (processAssign arg1 arg2)
        | Node2( "globalAssign", Variable( arg1), Leaf( arg2))    
                                               -> (processGlobalAsign arg1 arg2)
        | Node2( "globalMultiAssign", arg1, arg2)  
                                               -> (recursivePath arg1); (recursivePath arg2)
        | Node2("if", arg1, arg2)              -> (processIf arg1 arg2)
        | Node3("ifElse", arg1, arg2, arg3)    -> (processIfElse arg1 arg2 arg3) 
        | Node2("while", arg1, arg2)           -> (processWhile arg1 arg2)
        | Node2("MainWithGlobalVars", arg1, arg2)        
                                                -> (recursivePath arg1); (recursivePath arg2)
        
        | Node2("+=", Variable(arg1), arg2) -> (processIncrement arg1 (recursivePath arg2))
        | Node2("-=", Variable(arg1), arg2) -> (processIncrement arg1 (-(recursivePath arg2)))
        | Node2("*=", Variable(arg1), arg2) -> (processTimes arg1 (recursivePath arg2))
        | Node2("/=", Variable(arg1), arg2) -> (processDivide arg1 (recursivePath arg2))
        | Node1("-", arg1)                     -> -(recursivePath arg1)
        | Node2("+", arg1, arg2)                -> (recursivePath arg1) + (recursivePath arg2)
        | Node2("-", arg1, arg2)                -> (recursivePath arg1) - (recursivePath arg2)
        | Node2("*", arg1, arg2)                -> (recursivePath arg1) * (recursivePath arg2)
        | Node2("/", arg1, arg2)                -> (recursivePath arg1) / (recursivePath arg2)
        | Node2("%", arg1, arg2)                -> (recursivePath arg1) mod (recursivePath arg2)
        | Node2("<", arg1, arg2)                -> if ((recursivePath arg1) < (recursivePath arg2)) then 1 else 0
        | Node2(">", arg1, arg2)                -> if ((recursivePath arg1) > (recursivePath arg2)) then 1 else 0
        | Node2("<=", arg1, arg2)               -> if ((recursivePath arg1) <= (recursivePath arg2)) then 1 else 0
        | Node2(">=", arg1, arg2)               -> if ((recursivePath arg1) >= (recursivePath arg2)) then 1 else 0
        | Node2("==", arg1, arg2)               -> if ((recursivePath arg1) == (recursivePath arg2)) then 1 else 0
        | Node2("!=", arg1, arg2)               -> if ((recursivePath arg1) != (recursivePath arg2)) then 1 else 0
        | Node2("&&", arg1, arg2)               -> if ( ((recursivePath arg1) != 0) && ((recursivePath arg2) != 0) ) then 1 else 0
        | Node2("||", arg1, arg2)               -> if ( ((recursivePath arg1) != 0) ||  ((recursivePath arg2) != 0) ) then 1 else 0
        | Node1(arg1,arg2)                      -> print_string "Node1 Syntax error"; (exit 0)
        | Node2(arg1,arg2,arg3)                 -> print_string "Node2 Syntax error"; (exit 0)
        | Node3(arg1,arg2,arg3,arg4)            -> print_string "Node3 Syntax error"; (exit 0)
;;

(* Store input in a map *)
let storeInput = 
    (* store the first 2 lines that contains information of stream *)
    numberOfStreams := read_int();
    globalVars := (VariableBinding.add "NUMBER_OF_STREAMS" !numberOfStreams !globalVars);
    streamLength := read_int();
    globalVars := (VariableBinding.add "STREAM_LENGTH" !streamLength !globalVars); 

(* Read line by line and return a list containing refs to each int stream *)
let rec collect_lists count = match count with
    | 0 -> []
    | n -> ref (List.map int_of_string (Str.split (Str.regexp " ") (read_line ()))) :: (collect_lists (count - 1)) in
    (* Then consume the rest, return inputStream which is a list of ref *)
    inputStream := List.rev (collect_lists !numberOfStreams); (* Reverse the list so the entries for stream are in correct order *)
    0
;;

(* Create keys in the map, each key point to element of the head of the stream *)
let rec bindStreamElements n inStream = 
    let addElementToMap name value = streamBinding := (VariableBinding.add name value !streamBinding) in
    match !inStream with 
    | [] -> ref ()
    | car :: cdr -> addElementToMap ("#" ^ (string_of_int n)) (hd !car); (*  *) 
                    car := (tl !car);
                    let theRest = ref cdr in 
                      bindStreamElements (n + 1) theRest
;;

(* Print a list to the specifired format to stdout *)
let rec printList alist = match alist with
  | [] -> ()
  | hd::tl -> (Printf.printf "%d " hd); printList tl
;;

(* Run the code                                                                           *)
(* Read program, collects the input stream, then runs it STREAM_LENGTH times *)
let run =
storeInput;
   try
       let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
       (*try *)
           while true do
               let result = (Parser.main Lexer.token lexbuf) in
                entryPoint := result;
                for i = 0 to !streamLength-1 do
                    streamBinding := VariableBinding.empty;
                    (bindStreamElements 0 inputStream);
                    flush stdout;
                    recursivePath !entryPoint;
                done;
           done
   with Lexer.Eof ->
       print_int (List.length !outputStream); 
       print_string "\n";
       printList (List.rev !outputStream); 
       print_string "\n"; print_string "\n";
       exit 0

