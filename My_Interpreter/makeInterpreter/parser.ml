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

open Parsing;;
# 3 "parser.mly"
  open ParseTree;;
  open Lexer;;
# 47 "parser.ml"
let yytransl_const = [|
  259 (* SEMI_COLON *);
  260 (* BEGIN *);
  261 (* PLUS *);
  262 (* MINUS *);
  263 (* TIMES *);
  264 (* DIV *);
  265 (* EXP *);
  266 (* MOD *);
  267 (* INCRE *);
  268 (* DECRE *);
  269 (* INCRE_EQUAL *);
  270 (* DECRE_EQUAL *);
  271 (* ASSIGN *);
  272 (* GREATER *);
  273 (* GREATER_OR_EQUAL *);
  274 (* LESSER *);
  275 (* LESSER_OR_EQUAL *);
  276 (* EQUAL *);
  277 (* NOT_EQUAL *);
  278 (* OR *);
  279 (* AND *);
  280 (* NOT *);
  281 (* LPAREN *);
  282 (* RPAREN *);
  283 (* LCURLYB *);
  284 (* RCURLYB *);
  285 (* OPENSTREAM *);
  286 (* CLOSESTREAM *);
  287 (* PRINT *);
  288 (* IF *);
  289 (* THEN *);
  290 (* ELSE *);
  291 (* ELSE_IF *);
  292 (* TRUE *);
  293 (* FALSE *);
  294 (* WHILE *);
  295 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\006\000\006\000\007\000\007\000\
\005\000\005\000\008\000\009\000\009\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\000\000"

let yylen = "\002\000\
\004\000\005\000\004\000\005\000\001\000\002\000\001\000\003\000\
\007\000\011\000\007\000\005\000\005\000\002\000\004\000\004\000\
\004\000\001\000\001\000\001\000\001\000\003\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\003\000\
\003\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\007\000\000\000\000\000\043\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\041\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\000\023\000\024\000\000\000\
\000\000\000\000\000\000\006\000\014\000\001\000\000\000\000\000\
\000\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\004\000\000\000\000\000\016\000\017\000\015\000\000\000\000\000\
\000\000\000\000\000\000\011\000\000\000\000\000\000\000\010\000"

let yydgoto = "\002\000\
\006\000\007\000\022\000\023\000\024\000\025\000\017\000\027\000\
\000\000"

let yysindex = "\005\000\
\014\255\000\000\000\000\230\254\021\255\000\000\003\255\253\254\
\070\255\000\000\021\255\021\255\021\255\000\000\000\000\201\255\
\249\254\242\254\021\255\245\254\246\254\070\255\030\000\000\000\
\247\254\199\000\000\000\186\000\154\000\220\255\021\255\021\255\
\021\255\021\255\021\255\021\255\021\255\021\255\021\255\021\255\
\021\255\021\255\021\255\021\255\000\000\000\000\000\000\070\255\
\051\000\021\255\021\255\000\000\000\000\000\000\021\255\021\255\
\021\255\000\000\171\000\171\000\186\000\186\000\154\000\154\000\
\154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
\248\254\022\255\242\255\008\000\072\000\093\000\114\000\000\000\
\000\000\254\254\255\254\000\000\000\000\000\000\070\255\070\255\
\016\255\024\255\025\255\000\000\029\255\070\255\032\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\106\255\000\000\000\000\000\000\000\000\035\255\000\000\000\000\
\000\000\135\000\000\000\074\255\075\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\144\255\149\255\132\255\138\255\127\255\130\255\
\139\255\156\255\158\255\163\255\164\255\169\255\170\255\175\255\
\000\000\061\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\246\255\000\000\254\255\000\000\236\255\255\255\000\000\
\000\000"

let yytablesize = 470
let yytable = "\008\000\
\009\000\052\000\016\000\046\000\047\000\001\000\018\000\026\000\
\028\000\029\000\030\000\019\000\048\000\050\000\051\000\003\000\
\049\000\004\000\054\000\080\000\026\000\010\000\003\000\003\000\
\087\000\088\000\011\000\073\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\071\000\072\000\005\000\091\000\012\000\013\000\026\000\075\000\
\076\000\005\000\005\000\092\000\077\000\078\000\079\000\094\000\
\014\000\015\000\093\000\096\000\009\000\009\000\005\000\081\000\
\003\000\009\000\089\000\090\000\000\000\000\000\010\000\003\000\
\008\000\095\000\000\000\011\000\031\000\034\000\031\000\031\000\
\031\000\031\000\000\000\009\000\009\000\026\000\026\000\009\000\
\009\000\000\000\000\000\009\000\026\000\012\000\013\000\009\000\
\009\000\009\000\005\000\031\000\034\000\020\000\000\000\031\000\
\034\000\014\000\015\000\021\000\021\000\000\000\021\000\021\000\
\021\000\021\000\021\000\021\000\000\000\000\000\000\000\000\000\
\000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\030\000\000\000\021\000\029\000\000\000\027\000\021\000\
\027\000\027\000\027\000\027\000\028\000\037\000\028\000\028\000\
\028\000\028\000\025\000\000\000\025\000\025\000\000\000\026\000\
\030\000\026\000\026\000\029\000\030\000\027\000\038\000\029\000\
\039\000\027\000\000\000\028\000\037\000\040\000\035\000\028\000\
\037\000\025\000\000\000\036\000\033\000\025\000\026\000\000\000\
\000\000\032\000\026\000\000\000\000\000\038\000\000\000\039\000\
\000\000\038\000\000\000\039\000\040\000\035\000\000\000\000\000\
\040\000\035\000\036\000\033\000\000\000\000\000\036\000\033\000\
\032\000\000\000\000\000\000\000\032\000\031\000\032\000\033\000\
\034\000\035\000\036\000\000\000\000\000\000\000\000\000\000\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\031\000\032\000\033\000\034\000\035\000\036\000\045\000\000\000\
\000\000\000\000\000\000\037\000\038\000\039\000\040\000\041\000\
\042\000\043\000\044\000\000\000\000\000\058\000\031\000\032\000\
\033\000\034\000\035\000\036\000\000\000\000\000\000\000\000\000\
\000\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\000\000\000\000\082\000\031\000\032\000\033\000\034\000\
\035\000\036\000\000\000\000\000\000\000\000\000\000\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\000\000\
\053\000\083\000\031\000\032\000\033\000\034\000\035\000\036\000\
\000\000\000\000\000\000\000\000\000\000\037\000\038\000\039\000\
\040\000\041\000\042\000\043\000\044\000\074\000\000\000\031\000\
\032\000\033\000\034\000\035\000\036\000\000\000\000\000\000\000\
\000\000\000\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\084\000\000\000\031\000\032\000\033\000\034\000\
\035\000\036\000\000\000\000\000\000\000\000\000\000\000\037\000\
\038\000\039\000\040\000\041\000\042\000\043\000\044\000\085\000\
\000\000\031\000\032\000\033\000\034\000\035\000\036\000\000\000\
\000\000\000\000\000\000\000\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\086\000\000\000\031\000\032\000\
\033\000\034\000\035\000\036\000\000\000\000\000\000\000\000\000\
\000\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\021\000\000\000\021\000\021\000\021\000\021\000\021\000\
\021\000\000\000\000\000\000\000\000\000\000\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\031\000\032\000\
\033\000\034\000\035\000\036\000\000\000\000\000\000\000\000\000\
\000\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\033\000\034\000\035\000\036\000\000\000\000\000\000\000\
\000\000\000\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\035\000\036\000\000\000\000\000\000\000\000\000\
\000\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\046\000\047\000\055\000\056\000\057\000"

let yycheck = "\001\000\
\027\001\022\000\005\000\011\001\012\001\001\000\004\001\009\000\
\011\000\012\000\013\000\015\001\027\001\025\001\025\001\002\001\
\019\000\004\001\028\001\028\001\022\000\001\001\002\001\002\001\
\027\001\027\001\006\001\048\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\029\001\028\001\024\001\025\001\048\000\050\000\
\051\000\029\001\029\001\028\001\055\000\056\000\057\000\027\001\
\036\001\037\001\034\001\028\001\001\001\002\001\028\001\074\000\
\004\001\006\001\087\000\088\000\255\255\255\255\001\001\002\001\
\074\000\094\000\255\255\006\001\003\001\003\001\005\001\006\001\
\007\001\008\001\255\255\024\001\025\001\087\000\088\000\028\001\
\029\001\255\255\255\255\032\001\094\000\024\001\025\001\036\001\
\037\001\038\001\029\001\026\001\026\001\032\001\255\255\030\001\
\030\001\036\001\037\001\038\001\003\001\255\255\005\001\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\003\001\255\255\026\001\003\001\255\255\003\001\030\001\
\005\001\006\001\007\001\008\001\003\001\003\001\005\001\006\001\
\007\001\008\001\003\001\255\255\005\001\006\001\255\255\003\001\
\026\001\005\001\006\001\026\001\030\001\026\001\003\001\030\001\
\003\001\030\001\255\255\026\001\026\001\003\001\003\001\030\001\
\030\001\026\001\255\255\003\001\003\001\030\001\026\001\255\255\
\255\255\003\001\030\001\255\255\255\255\026\001\255\255\026\001\
\255\255\030\001\255\255\030\001\026\001\026\001\255\255\255\255\
\030\001\030\001\026\001\026\001\255\255\255\255\030\001\030\001\
\026\001\255\255\255\255\255\255\030\001\005\001\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\005\001\006\001\007\001\008\001\009\001\010\001\030\001\255\255\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\026\001\005\001\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\255\255\255\255\026\001\005\001\006\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\255\255\
\003\001\026\001\005\001\006\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\003\001\255\255\005\001\
\006\001\007\001\008\001\009\001\010\001\255\255\255\255\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\003\001\255\255\005\001\006\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\003\001\
\255\255\005\001\006\001\007\001\008\001\009\001\010\001\255\255\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\003\001\255\255\005\001\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\003\001\255\255\005\001\006\001\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\255\255\255\255\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\005\001\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\007\001\008\001\009\001\010\001\255\255\255\255\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\011\001\012\001\013\001\014\001\015\001"

let yynames_const = "\
  SEMI_COLON\000\
  BEGIN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  EXP\000\
  MOD\000\
  INCRE\000\
  DECRE\000\
  INCRE_EQUAL\000\
  DECRE_EQUAL\000\
  ASSIGN\000\
  GREATER\000\
  GREATER_OR_EQUAL\000\
  LESSER\000\
  LESSER_OR_EQUAL\000\
  EQUAL\000\
  NOT_EQUAL\000\
  OR\000\
  AND\000\
  NOT\000\
  LPAREN\000\
  RPAREN\000\
  LCURLYB\000\
  RCURLYB\000\
  OPENSTREAM\000\
  CLOSESTREAM\000\
  PRINT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSE_IF\000\
  TRUE\000\
  FALSE\000\
  WHILE\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 40 "parser.mly"
                                              ( _3 )
# 334 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : ParseTree.parseTree) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 41 "parser.mly"
                                              ( Node2("MainWithGlobalVars", _1, _4) )
# 342 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 45 "parser.mly"
                                                    ( Node2("globalAssign", _1, _3) )
# 350 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 46 "parser.mly"
                                                    ( Node2("globalMultiAssign", Node2("globalAssign", _1, _3), _5))
# 359 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 50 "parser.mly"
                                              ( Node1("bodyOneSentence", _1) )
# 366 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 51 "parser.mly"
                                              ( Node2("bodyManySentences", _1, _2) )
# 374 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                                              ( Variable( _1 ))
# 381 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 57 "parser.mly"
                                              ( Node1("streamValue", _2) )
# 388 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : ParseTree.parseTree) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 64 "parser.mly"
                                                                                   ( Node2("if", _3, _6) )
# 396 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : ParseTree.parseTree) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : ParseTree.parseTree) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 65 "parser.mly"
                                                                                   ( Node3("if", _3, _6, _10) )
# 405 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : ParseTree.parseTree) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 72 "parser.mly"
                                                                ( Node2 ("while", _3, _6) )
# 413 "parser.ml"
               : 'while_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    Obj.repr(
# 77 "parser.mly"
                                                    ( Node1("print", _3) )
# 420 "parser.ml"
               : 'print))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    Obj.repr(
# 78 "parser.mly"
                                                    ( Node1("printVariable", _3) )
# 427 "parser.ml"
               : 'print))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 81 "parser.mly"
                                       ( _1 )
# 434 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 82 "parser.mly"
                                                  ( Node2("assign", _1, _3) )
# 442 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 83 "parser.mly"
                                                  ( Node2("+=", _1, _3) )
# 450 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 84 "parser.mly"
                                                  ( Node2("-=", _1, _3) )
# 458 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 85 "parser.mly"
                                                  ( _1 )
# 465 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'while_statement) in
    Obj.repr(
# 86 "parser.mly"
                                                  ( _1 )
# 472 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "parser.mly"
                                              ( Leaf(_1) )
# 479 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 95 "parser.mly"
                                              ( _1 )
# 486 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : ParseTree.parseTree) in
    Obj.repr(
# 96 "parser.mly"
                                              ( _2 )
# 493 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    Obj.repr(
# 98 "parser.mly"
                                              ( Node1("++", _1) )
# 500 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    Obj.repr(
# 99 "parser.mly"
                                              ( Node1("--", _1) )
# 507 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 101 "parser.mly"
                                              ( Node2("+", _1, _3) )
# 515 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 102 "parser.mly"
                                              ( Node2("-", _1, _3) )
# 523 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 103 "parser.mly"
                                              ( Node2("*", _1, _3) )
# 531 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 104 "parser.mly"
                                              ( Node2("/", _1, _3) )
# 539 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 105 "parser.mly"
                                              ( Node2("%", _1, _3) )
# 547 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 106 "parser.mly"
                                              ( Node2("^", _1, _3) )
# 555 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 107 "parser.mly"
                                              ( Node1("-",_2) )
# 562 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 109 "parser.mly"
                                              ( Node2("&&", _1, _3) )
# 570 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 110 "parser.mly"
                                              ( Node2("||", _1, _3) )
# 578 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 111 "parser.mly"
                                              ( Node1("!", _2 ) )
# 585 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 112 "parser.mly"
                                              ( Node2("=", _1, _3) )
# 593 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 113 "parser.mly"
                                              ( Node2("!=", _1, _3) )
# 601 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 114 "parser.mly"
                                              ( Node2(">", _1, _3) )
# 609 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 115 "parser.mly"
                                              ( Node2(">=", _1, _3) )
# 617 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 116 "parser.mly"
                                              ( Node2("<", _1, _3) )
# 625 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ParseTree.parseTree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ParseTree.parseTree) in
    Obj.repr(
# 117 "parser.mly"
                                              ( Node2("<=", _1, _3) )
# 633 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
                                              ( Leaf (1) )
# 639 "parser.ml"
               : ParseTree.parseTree))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                                              ( Leaf (0) )
# 645 "parser.ml"
               : ParseTree.parseTree))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ParseTree.parseTree)
