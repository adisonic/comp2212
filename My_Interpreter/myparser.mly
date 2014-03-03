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
main:                            /* need to edit could be just a body program or inclue global variable */
                   { $1 }
;

expr
;

/* like expression, evaluate to a rawvalue */
rawvalue:
   INT                            { $1 }
 | LPAREN rawvalue RPAREN           { $2 }
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
   | cond_statement
   | while_statement   
   | sentence
;

variable:
   | STRING 

sentence: /* is a statement form a complete instruction, could include a semicolon (?) */
   | rawvalue SEMI_COLON
   | variable ASSIGN rawvalue SEMI_COLON
   | cond_statement
   | while_statement
   | 



/* if ( value ) { body } 
   if ( value ) { body } else { body } 
   if ( value ) { body } else_if*/
cond_statement: 
   IF condition LCURLYB body RCURLYB 
   | 
   /*| condstatement ELSE LCURLYB body RCURLYB
   | condstatement ELSE_IF LCURLYB body RCURLYB */ 

condition:
   LPAREN rawvalue comparison_operator rawvalue RPAREN 
   ;

/* while ( value ) { body } */
while_statement: /* loop */


   
comparison_operator: /* Equality and Relational Operators */
    EQUAL 
   | NOT_EQUAL
   | GREATER
   | GREATER_OR_EQUAL
   | LESSER 
   | LESSER_OR_EQUAL 
;

conditional_operator: 
   AND
   | OR
;
   
   

