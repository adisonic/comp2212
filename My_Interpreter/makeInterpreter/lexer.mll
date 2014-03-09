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
    | '%'         { MOD } (* end of categ *)
    | "begin"     { BEGIN }
    | ";"         { SEMI_COLON }               
    | '='         { ASSIGN } (* end of categ *)
    | '>'         { GREATER } 
    | ">="        { GREATER_OR_EQUAL } 
    | '<'         { LESSER }
    | "<="        { LESSER_OR_EQUAL }(* end of categ *)
    | "++"        { INCRE }
    | "--"        { DECRE } 
    | "+="        { INCRE_EQUAL }
    | "-="        { DECRE_EQUAL }
    | "=="        { EQUAL }
    | "!="        { NOT_EQUAL } (* end of categ *)
    | '!'         { NOT }
    | "||"        { OR }
    | "&&"        { AND } (* end of categ *)
    | '('         { LPAREN }
    | ')'         { RPAREN }
    | '{'         { LCURLYB }
    | '}'         { RCURLYB }  
    | "print"     { PRINT}
    | "stream["   { OPENSTREAM }
    | ']'         { CLOSESTREAM }  (* end of categ *)
    | "if"        { IF }
    | "then"      { THEN } (* use in if ... then ... else, but maybe not needed *)
    | "else"      { ELSE } 
    | "while"     { WHILE }
    | "true"      { TRUE }
    | "false"     { FALSE }  (* end of categ *)
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as id { STRING( id ) } (* identifier *)
    | eof         { raise Eof }
  
