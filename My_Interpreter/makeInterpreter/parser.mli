type token =
  | INT of (int)
  | STRING of (string)
  | SEMI_COLON
  | BEGIN
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EXP
  | MOD
  | INCRE
  | DECRE
  | INCRE_EQUAL
  | DECRE_EQUAL
  | ASSIGN
  | GREATER
  | GREATER_OR_EQUAL
  | LESSER
  | LESSER_OR_EQUAL
  | EQUAL
  | NOT_EQUAL
  | OR
  | AND
  | NOT
  | LPAREN
  | RPAREN
  | LCURLYB
  | RCURLYB
  | OPENSTREAM
  | CLOSESTREAM
  | PRINT
  | IF
  | THEN
  | ELSE
  | ELSE_IF
  | TRUE
  | FALSE
  | WHILE
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ParseTree.parseTree
