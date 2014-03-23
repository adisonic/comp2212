/* File parser.mly */
%{
open ParseTree;;
%}
%token <int> INT
%token <string> STRING
%token PLUS MINUS TIMES DIVIDE MOD
%token LPAREN RPAREN
%token SEMI_COLON
%token LCURLYB RCURLYB
%token IF ELSE WHILE
%token PRINT
%token LESSER GREATER LESSER_EQUAL GREATER_EQUAL 
%token EQUAL NOT_EQUAL
%token NOT OR AND
%token ASSIGN
%token BEGIN
%token OPENSTREAM CLOSESTREAM
%token TRUE FALSE
%token INCRE DECRE PLUS_EQUAL MINUS_EQUAL TIMES_EQUAL DIVIDE_EQUAL
%left PLUS MINUS        
%left TIMES DIV         
%nonassoc UMINUS        

%start main             
%type <ParseTree.parseTree> main
%type <ParseTree.parseTree> rawvalue
%type <ParseTree.parseTree> sentence
%type <ParseTree.parseTree> body
%type <ParseTree.parseTree> cond_stamement
%type <ParseTree.parseTree> globalVars
%%

/* Program start here */
main:
 | BEGIN LCURLYB body RCURLYB 			   { $3 }
 | globalVars BEGIN LCURLYB body RCURLYB   { Node2("MainWithGlobalVars", $1, $4) }
;

/* one or many flobarVars */
globalVars:
 | varaiable ASSIGN rawvalue SEMI_COLON               { Node2("globalAssign", $1, $3) }
 | varaiable ASSIGN rawvalue SEMI_COLON globalVars    { Node2("globalMultiAssign", Node2("globalAssign", $1, $3), $5) }
;

/* Body, a list of sentences , inside curly brakets to run */
body:
   sentence                             { Node1("bodyOneSentence", $1) }
 | sentence body                 		{ Node2("bodyManySentences", $1, $2) }
;

/* Variable names */
varaiable:
 |  STRING                      					 { Variable( $1 ) }
 |  OPENSTREAM rawvalue CLOSESTREAM 		    	 { Node1("streamValue", $2) }
;

/* Conditional statements (Note they must always have braces) */
cond_stamement:
   IF LPAREN rawvalue RPAREN LCURLYB body RCURLYB                                  { Node2("if", $3, $6) }
 | IF LPAREN rawvalue RPAREN LCURLYB body RCURLYB ELSE LCURLYB body RCURLYB   	   { Node3("ifElse", $3, $6, $10) }
 
;

while_statement:
   WHILE LPAREN rawvalue RPAREN LCURLYB body RCURLYB  { Node2("while", $3, $6) }
;

/* print elements to the stdout */
print:
 | PRINT LPAREN rawvalue RPAREN SEMI_COLON       { Node1("print", $3) } 
 | PRINT LPAREN varaiable RPAREN SEMI_COLON      { Node1("printVariable", $3) }
;

/* is a statement form a complete instruction, could include a semicolon  */
sentence:
   rawvalue SEMI_COLON            	     	   { $1 }
 | cond_stamement                     	       { $1 }
 | while_statement                     		   { $1 }
 | print                     			       { $1 }
 | varaiable ASSIGN rawvalue SEMI_COLON        { Node2("assign", $1, $3) }
 | varaiable PLUS_EQUAL rawvalue SEMI_COLON    { Node2("+=", $1, $3) }
 | varaiable MINUS_EQUAL rawvalue SEMI_COLON   { Node2("-=", $1, $3) }
 | varaiable TIMES_EQUAL rawvalue SEMI_COLON   { Node2("*=", $1, $3) }
 | varaiable DIVIDE_EQUAL rawvalue SEMI_COLON  { Node2("/=", $1, $3) }         
;

/* Like expression, evaluate to a some value  */
/*Include: arithmetic operations, comparision operations, something that could be avaluated to 1 or 0 (boolean)
  No semi-colon so that can be recursively evaluated it it's a tree*/
rawvalue:
   INT                        		 { Leaf($1) }
 | LPAREN rawvalue RPAREN        	 { Node1("value", $2) }
 | rawvalue PLUS rawvalue            { Node2("+", $1, $3) }
 | rawvalue MINUS rawvalue           { Node2("-", $1, $3) }
 | rawvalue TIMES rawvalue           { Node2("*", $1, $3) }
 | rawvalue DIVIDE rawvalue          { Node2("/", $1, $3) }
 | rawvalue MOD rawvalue			 { Node2("%", $1, $3) }
 | MINUS rawvalue %prec UMINUS 	     { Node1("-", $2) }
 | TRUE                    		     { Leaf(1) }
 | FALSE                    	     { Leaf(0) }
 | varaiable                   		 { $1 }
 | rawvalue LESSER rawvalue          { Node2("<", $1, $3) }
 | rawvalue GREATER rawvalue         { Node2(">", $1, $3) }
 | rawvalue LESSER_EQUAL rawvalue    { Node2("<=", $1, $3) }
 | rawvalue GREATER_EQUAL rawvalue   { Node2(">=", $1, $3) }
 | rawvalue EQUAL rawvalue           { Node2("==", $1, $3) }
 | rawvalue NOT_EQUAL rawvalue       { Node2("!=", $1, $3) }
 | rawvalue OR rawvalue              { Node2("||", $1, $3) }
 | rawvalue AND rawvalue             { Node2("&&", $1, $3) }
 | NOT rawvalue                  	 { Node1("!", $2) }
 | varaiable INCRE                   { Node1("++", $1) }
 | varaiable DECRE                   { Node1("--", $1) }
;
