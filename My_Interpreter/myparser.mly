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


/*NEED TO ADD RELEVANT STUFF TO TYPE PARSETREE BELOW */
%type <myParseTree.parseTree> main
%type <myParseTree.parseTree> globalVars
%type <myParseTree.parseTree>
%type <myParseTree.parseTree>
%type <myParseTree.parseTree>
%type <myParseTree.parseTree>
%type <myParseTree.parseTree>
%%


main:                            /* need to edit could be just a body program or inclue global variable */
  BEGIN LCURLYB body RCURLYB                  { $3 }
  | globalVars BEGIN LCURLYB body RCURLYB     { Node2("MainWithGlobalVars", $1, $3)}                
;

globalVars: 
 | variable ASSIGN rawvalue SEMICOLON              { Node2("AssignGlobalVars", $1, $3)}
 | variable ASSIGN rawvalue SEMICOLON globalVars   { Node2("MultipleGlobalVars", Node2("AssignGlobalVars", $1, $3), $5}
;
  

/* like expression, evaluate to a rawvalue, no semi-colon */
/*Include: arithmetic operations
           comparision operations: something that could be avaluated to 1 or 0 (boolean) */
rawvalue:
   INT                                        { Leaf($1) }
 | LPAREN rawvalue RPAREN                     { $2 }
 
 | variable INCRE /* ++ */                    { Node1("Increment", $1) }  
 | variable DECRE /* -- */                    { Node1("Decrement", $1) }
 
 | rawvalue PLUS rawvalue                     { Node2("Plus", $1, $3) }
 | rawvalue MINUS rawvalue                    { Node2("Minus", $1, $3) }
 | rawvalue TIMES rawvalue                    { Node2("Times", $1, $3) }        
 | rawvalue DIV rawvalue                      { Node2("Divide", $1, $3) }         
 | rawvalue MOD rawvalue                      { Node2("Modulus", $1, $3) }
 | rawvalue EXP rawvalue                      { Node2("Exponential", $1, $3) }
 | MINUS rawvalue %prec UMINUS
  
 | rawvalue AND rawvalue                      { 
 | rawvalue OR rawvalue 
 | rawvalue EQUAL rawvalue 
 | rawvalue NOT_EQUAL rawvalue
 | rawvalue GREATER rawvalue
 | rawvalue GREATER_OR_EQUAL rawvalue
 | rawvalue LESSER rawvalue
 | rawvalue LESSER_OR_EQUAL rawvalue
 | TRUE                                       /* Think we hould represent it as a leaf string */
 | FALSE 
 | variable

; 
   
body:
     sentence                   { Node
     | sentence body
;

variable:
   | STRING 

sentence: /* is a statement form a complete instruction, could include a semicolon (?) */
   | rawvalue SEMI_COLON            /*Do we need? */   { $1 }              
   | variable ASSIGN rawvalue SEMI_COLON          { Node2("Assign", $1, $3) }  
   | variable INCRE_EQUAL rawvalue SEMI_COLON     { Node2("Decrement", $1, $3) }
   | variable DECRE_EQUAL rawvalue SEMI_COLON     { Node2("Decrement", $1, $3) }
   | cond_statement                               { $1 }
   | while_statement                              { $1 }  
   

/* if ( value ) { body } 
   if ( value ) { body } else { body } 
   if ( value ) { body } else_if */
   
cond_statement: 
   IF condition LCURLYB body RCURLYB                                  { Node2("if", $2, $4) }
   | IF condition LCURLYB body RCURLYB ELSE LCURLYB body RCURLYB      { Node3("ifelse", $2, $4, $8) }
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
   
   


