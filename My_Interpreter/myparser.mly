/* File parser.mly */
open ParseTree

%token <int> INT
%token <string> STRING
%token SEMI_COLON
%token PLUS MINUS TIMES DIV EXP MOD  
%token INCRE DECRE INCRE_EQUAL DECRE_EQUAL
%token ASSIGN
%token GREATER GREATER_OR_EQUAL LESSER LESSER_OR_EQUAL EQUAL NOT_EQUAL
%token OR AND 
%token NOT
%token LPAREN RPAREN LCURLYB RCURLYB
%token OPENSTREAM CLOSESTREAM 
%token IF THEN ELSE ELSE_IF
%token TRUE FALSE
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%start main             /* the entry point */
%type <int> main
%%
main:
   rawvalue EOL                { $1 }
;
expr

rawvalue:
   INT                     { $1 }
 | LPAREN rawvalue RPAREN      { $2 }
 | rawvalue PLUS rawvalue          { $1 + $3 }
 | rawvalue MINUS rawvalue         { $1 - $3 }
 | rawvalue TIMES rawvalue         { $1 * $3 }
 | rawvalue DIV rawvalue           { $1 / $3 }
 | rawvalue MOD rawvalue           { $1 ?? $3}
 | rawvalue EXP rawvalue           { $1 ** $3}
 
 | MINUS rawvalue %prec UMINUS { - $2 }
 | value 
;

body:
   
   
   
condstatement:
   IF comparator LCURLYB body RCURLYB 
   | condstatement ELSE LCURLYB body RCURLYB
   | condstatement ELSE_IF LCURLYB body RCURLYB

comparator:
   LPAREN rawvalue condition rawvalue RPAREN 
   ;
   
condition:
   GREATER
   | GREATER_OR_EQUAL
   | LESSER 
   | LESSER_OR_EQUAL 
   | EQUAL 
   | NOT_EQUAL
   
   

