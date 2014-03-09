main:
  BEGIN LCURLYB body RCURLYB { $3 }
  | globalVars BEGIN LCURLYB body RCURLYB { Node2("MainWithGlobalVars", $1, $4)} (* done *)
;

globalVars:
 | STRING ASSIGN rawvalue SEMI_COLON { Node2("globalAssign", Variable($1), $3)} (* done *)
 | STRING ASSIGN rawvalue SEMI_COLON globalVars { Node2("globalAssignExtending", Node2("globalAssign", $1, $3), $5)} 
                                                (*Check main to see if handled properly*)
;
  
body:
     sentence { Node1("bodyEnding", $1) } (* done *)
     | sentence body { Node2("bodyExtend", $1, $2) } (* done *)
;

/* binding: variable and its value */

variable:
   | STRING { Variable($1)} (*done *)
   | OPENSTREAM rawvalue CLOSESTREAM { Node1("streamValue", $2) } (* done *)

/* if ( value ) { body }
   if ( value ) { body } else { body }
   if ( value ) { body } else_if */
   
cond_statement:
   IF LPAREN rawvalue RPAREN LCURLYB body RCURLYB { Node2("if", $3, $6) } (* done *)
   | IF LPAREN rawvalue RPAREN LCURLYB body RCURLYB ELSE LCURLYB body RCURLYB { Node3("if", $3, $6, $10) } (* done *)
   /* May need to add elseif */

                    

/* while ( value ) { body } */
while_statement: /* loop */
   | WHILE LPAREN rawvalue RPAREN LCURLYB body RCURLYB { Node2 ("while", $3, $6) } (* done *)
;
   
   
sentence: /* is a statement form a complete instruction, could include a semicolon (?) */
   | rawvalue SEMI_COLON { $1 }
   | variable ASSIGN rawvalue SEMI_COLON { Node2("assign", $1, $3) } (* done *)
   | variable INCRE_EQUAL rawvalue SEMI_COLON { Node2("+=", $1, $3) } (* done *)
   | variable DECRE_EQUAL rawvalue SEMI_COLON { Node2("-=", $1, $3) } (* done *) /* may need to add *= and /= */
   | cond_statement { $1 } (* done *)
   | while_statement { $1 } (* done *)
;
   
   
/* like expression, evaluate to a rawvalue, no semi-colon */
/*Include: arithmetic operations
           comparision operations: something that could be avaluated to 1 or 0 (boolean) */
rawvalue:
   INT { Leaf($1) }
 | LPAREN rawvalue RPAREN { $2 }
 
 | variable INCRE /* ++ */ { Node1("++", $1) } (* done *)
 | variable DECRE /* -- */ { Node1("--", $1) } (* done *)
 
 | rawvalue PLUS rawvalue { Node2("+", $1, $3) } (* done *)
 | rawvalue MINUS rawvalue { Node2("-", $1, $3) } (* done *)
 | rawvalue TIMES rawvalue { Node2("*", $1, $3) } (* done *)
 | rawvalue DIV rawvalue { Node2("/", $1, $3) } (* done *)
 | rawvalue MOD rawvalue { Node2("%", $1, $3) } (* done *)
 | rawvalue EXP rawvalue { Node2("^", $1, $3) } (* done *)
 | MINUS rawvalue %prec UMINUS { Node1("-", $2) }         (* done *)
  
 | rawvalue AND rawvalue { Node2("&&", $1, $3) } (* done *)
 | rawvalue OR rawvalue { Node2("||", $1, $3) } (* done *)
 | NOT rawvalue { Node1("!", $2 )} (* done *) (* done *)
 | rawvalue EQUAL rawvalue { Node2("==", $1, $3) } (* done *)
 | rawvalue NOT_EQUAL rawvalue { Node2("!=", $1, $3) } (* done *)
 | rawvalue GREATER rawvalue { Node2(">", $1, $3) } (* done *) 
 | rawvalue GREATER_OR_EQUAL rawvalue { Node2(">=", $1, $3) } (* done *)
 | rawvalue LESSER rawvalue { Node2("<", $1, $3) } (* done *)
 | rawvalue LESSER_OR_EQUAL rawvalue { Node2("<=", $1, $3) } (* done *)
 | TRUE { Leaf (1) }
 | FALSE { Leaf (0) }

