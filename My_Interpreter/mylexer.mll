(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
      [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]  { token lexbuf }
    | "/*" [^'|''*']* "*/"    { token lexbuf } 
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | '+'         { PLUS }
    | '-'         { MINUS }
    | '*'         { TIMES }
    | '/'         { DIV }
    | '^'         { EXP }
    | '%'         { MOD } 
    
    | "begin"     { BEGIN }
    
    
    | '='         { ASSIGN } 
    
    | '>'         { GREATER } 
    | ">="        { GREATER_OR_EQUAL } 
    | '<'         { LESSER }
    | "<="        { LESSER_OR_EQUAL }
    
    | "++"        { INCRE }
    | "--"        { DECRE } 
    | "+="        { INCRE_EQUAL }
    | "-="        { DECRE_EQUAL }
    
    | "=="        { EQUAL }
    | "!="        { NOT_EQUAL } 
    | '!'         { NOT }
    | "||"        { OR }
    | "&&"        { AND }
    | '('         { LPAREN }
    | ')'         { RPAREN }
    | '{'         { LCURLYB }
    | '}'         { RCURLYB }
    | "stream[["  { OPENSTREAM }
    | "]]"        { CLOSESTREAM }
    
    | "if"        { IF }
    | "then"      { THEN } 
    | "else"      { ELSE } 
    | "while"     { WHILE }
    | "else_if"   { ELSE_IF }
    | "true"      { TRUE }
    | "false"     { FALSE }
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as id { STRING( id ) } (* identifier *)
    
    | eof      { raise Eof }
  
