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
%type <myParseTree.parseTree> rawvalue
%type <myParseTree.parseTree> cond_statement
%type <myParseTree.parseTree> while_statement
%type <myParseTree.parseTree> sentence
/* May need to add more.. so revise if any errors
%type <myParseTree.parseTree>
%%


main:                            /* need to edit could be just a body program or inclue global variable */
  BEGIN LCURLYB body RCURLYB                  { $3 }
  | globalVars BEGIN LCURLYB body RCURLYB     { Node2("MainWithGlobalVars", $1, $3)}                
;

globalVars: 
 | variable ASSIGN rawvalue SEMICOLON              { Node2("globalAssign", $1, $3)}
 | variable ASSIGN rawvalue SEMICOLON globalVars   { Node2("globalAssignExtending", Node2("globalAssign", $1, $3), $5}
;
  
body:   
     sentence                                 { Node1("bodyEnding", $1) } 
     | sentence body                          { Node2("bodyExtend", $1, $2 }
;

/* binding: variable and its value */
variable:
   | STRING                                   { Variable($1)}
   | OPENSTREAM rawvalue CLOSESTREAM          { Node1("streamValue", $2) }

/* if ( value ) { body } 
   if ( value ) { body } else { body } 
   if ( value ) { body } else_if */
   
cond_statement: 
   IF LPAREN rawvalue RPAREN LCURLYB body RCURLYB                                  { Node2("if", $3, $6) }
   | IF LPAREN rawvalue RPAREN LCURLYB body RCURLYB ELSE LCURLYB body RCURLYB      { Node3("if", $3, $6, $10) }
   /* May need to add elseif */

                    

/* while ( value ) { body } */
while_statement: /* loop */
   | WHILE LPAREN rawvalue RPAREN LCURLYB body RCURLYB          { Node1 ("while", $3, $6) }                       
;
   
   
sentence: /* is a statement form a complete instruction, could include a semicolon (?) */
   | rawvalue SEMI_COLON            /*Do we need? */   { $1 }              
   | variable ASSIGN rawvalue SEMI_COLON          { Node2("assign", $1, $3) }  
   | variable INCRE_EQUAL rawvalue SEMI_COLON     { Node2("+=", $1, $3) }
   | variable DECRE_EQUAL rawvalue SEMI_COLON     { Node2("-=", $1, $3) } /* may need to add *= and /= */
   | cond_statement                               { $1 }
   | while_statement                              { $1 }  
;
   
   
/* like expression, evaluate to a rawvalue, no semi-colon */
/*Include: arithmetic operations
           comparision operations: something that could be avaluated to 1 or 0 (boolean) */
rawvalue:
   INT                                        { Leaf($1) }
 | LPAREN rawvalue RPAREN                     { $2 }
 
 | variable INCRE /* ++ */                    { Node1("++", $1) }  
 | variable DECRE /* -- */                    { Node1("--", $1) }
 
 | rawvalue PLUS rawvalue                     { Node2("+", $1, $3) }
 | rawvalue MINUS rawvalue                    { Node2("-", $1, $3) }
 | rawvalue TIMES rawvalue                    { Node2("*", $1, $3) }        
 | rawvalue DIV rawvalue                      { Node2("/", $1, $3) }         
 | rawvalue MOD rawvalue                      { Node2("%", $1, $3) }
 | rawvalue EXP rawvalue                      { Node2("^", $1, $3) }
 /*| MINUS rawvalue %prec UMINUS                { - $2 } */
  
 | rawvalue AND rawvalue                      { Node2("&&", $1, $3) }
 | rawvalue OR rawvalue                       { Node2("||", $1, $3) }
 | NOT rawvalue                               { Node1("!", $1 }
 | rawvalue EQUAL rawvalue                    { Node2("=", $1, $3) }
 | rawvalue NOT_EQUAL rawvalue                { Node2("!=", $1, $3) }
 | rawvalue GREATER rawvalue                  { Node2(">", $1, $3) }
 | rawvalue GREATER_OR_EQUAL rawvalue         { Node2(">=", $1, $3) }
 | rawvalue LESSER rawvalue                   { Node2("<", $1, $3) }
 | rawvalue LESSER_OR_EQUAL rawvalue          { Node2("<=", $1, $3) }
 | TRUE                                       { LeafBool (true) }         
 | FALSE                                      { LeafBool (false) }

;   
   


