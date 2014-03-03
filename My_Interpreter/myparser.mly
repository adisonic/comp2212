/* File parser.mly */
open MyParseTree

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
  BEGIN LCURLYB body RCURLYB
  | globalVars BEGIN LCURLYB body RCURLYB                      
;

globalVars: 
 | variable ASSIGN rawvalue SEMICOLON
 | variable ASSIGN rawvalue SEMICOLON globalVars
;
  

/* like expression, evaluate to a rawvalue, no semi-colon */
/*Include: arithmetic operations
           comparision operations: something that could be avaluated to 1 or 0 (boolean) */
rawvalue:
   INT                           
 | LPAREN rawvalue RPAREN   
 
 | rawvalue INCRE /* ++ */
 | rawvalue DECRE /* -- */  
 
 | rawvalue PLUS rawvalue        
 | rawvalue MINUS rawvalue        
 | rawvalue TIMES rawvalue        
 | rawvalue DIV rawvalue          
 | rawvalue MOD rawvalue           
 | rawvalue EXP rawvalue           
 | MINUS rawvalue %prec UMINUS
  
 | rawvalue AND rawvalue
 | rawvalue OR rawvalue 
 | rawvalue EQUAL rawvalue 
 | rawvalue NOT_EQUAL rawvalue
 | rawvalue GREATER rawvalue
 | rawvalue GREATER_OR_EQUAL rawvalue
 | rawvalue LESSER rawvalue
 | rawvalue LESSER_OR_EQUAL rawvalue
 | TRUE 
 | FALSE 
 | variable

; 
   
body:
     sentence
     | sentence body
;

variable:
   | STRING 

sentence: /* is a statement form a complete instruction, could include a semicolon (?) */
   | rawvalue SEMI_COLON
   | variable ASSIGN rawvalue SEMI_COLON
   | variable INCRE_EQUAL rawvalue SEMI_COLON
   | variable DECRE_EQUAL rawvalue SEMI_COLON   
   | cond_statement
   | while_statement
   

/* if ( value ) { body } 
   if ( value ) { body } else { body } 
   if ( value ) { body } else_if */
cond_statement: 
   IF condition LCURLYB body RCURLYB 
   | IF condition LCURLYB body RCURLYB ELSE LCURLYB body RCURLYB
   /* May need to add elseif */

 
condition:
   LPAREN rawvalue RPAREN 


/* while ( value ) { body } */
while_statement: /* loop */
   | WHILE condition LCURLYB body RCURLYB 

conditional_operator: 
   AND
   | OR
;
   
   


