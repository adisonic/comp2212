(* File lexer.mll *)
{
open Parser        (* Token list in parser.mli *)
exception Eof
}
rule token = parse
      [' ''\t']     { token lexbuf }     (* skip blanks *)
    | ['\n']    {token lexbuf}  
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | "/*" [^'|''*']* "*/" {token lexbuf}
    | '+'       { PLUS }
    | '-'       { MINUS }
    | '*'       { TIMES }
    | '/'       { DIVIDE }
    | '%'       { MOD }
    | "++"      { INCRE }
    | "--"      { DECRE }
    | "+="      { PLUS_EQUAL }
    | "-="      { MINUS_EQUAL }
    | "*="      { TIMES_EQUAL }
    | "/="      { DIVIDE_EQUAL }
    | '('       { LPAREN }
    | ')'       { RPAREN }
    | ';'       { SEMI_COLON }
    | '{'       { LCURLYB }
    | '}'       { RCURLYB } 
    | "if"      { IF }
    | "else"    { ELSE }
    | "while"   { WHILE }
    | "TRUE"    { TRUE }
    | "FALSE"   { FALSE }
    | "print"   { PRINT }
    | '<'       { LESSER }
    | "<="      { LESSER_EQUAL }
    | "||"      { OR }
    | "&&"      { AND }
    | '>'       { GREATER }
    | ">="      { GREATER_EQUAL }
    | "=="      { EQUAL }
    | "!="      { NOT_EQUAL }
    | '!'       { NOT }
    | "begin"   { BEGIN }
    | "stream[" { OPENSTREAM }
    | ']'       { CLOSESTREAM }
    | '='       { ASSIGN }
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as id { STRING( id ) } (* done *)
    | eof       { raise Eof }
