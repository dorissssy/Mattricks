type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | ASSIGN
  | DASSIGN
  | LBRAC
  | RBRAC
  | EQ
  | NEQ
  | LT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | RETURN
  | COMMA
  | LITERAL of (int)
  | FLIT of (float)
  | BLIT of (bool)
  | ID of (string)
  | EOF
  | PRINTF

open Parsing;;
let _ = parse_error;;
# 4 "nanocparse.mly"
open Ast
# 38 "nanocparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* PLUS *);
  263 (* MINUS *);
  264 (* ASSIGN *);
  265 (* DASSIGN *);
  266 (* LBRAC *);
  267 (* RBRAC *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* AND *);
  272 (* OR *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* WHILE *);
  276 (* INT *);
  277 (* BOOL *);
  278 (* FLOAT *);
  279 (* RETURN *);
  280 (* COMMA *);
    0 (* EOF *);
  285 (* PRINTF *);
    0|]

let yytransl_block = [|
  281 (* LITERAL *);
  282 (* FLIT *);
  283 (* BLIT *);
  284 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\005\000\005\000\005\000\003\000\
\003\000\006\000\006\000\006\000\006\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\003\000\001\000\001\000\001\000\000\000\
\002\000\002\000\003\000\007\000\005\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\007\000\010\000\006\000\
\008\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\005\000\006\000\007\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\015\000\014\000\
\000\000\000\000\000\000\000\000\000\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\009\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\027\000\011\000\000\000\000\000\000\000\000\000\
\000\000\000\000\018\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\013\000\
\000\000\000\000\000\000\000\000\032\000\000\000\012\000\000\000\
\000\000\000\000\033\000\000\000\031\000"

let yydgoto = "\002\000\
\006\000\007\000\019\000\008\000\009\000\020\000\021\000"

let yysindex = "\006\000\
\144\255\000\000\000\000\000\000\000\000\000\000\020\255\144\255\
\236\254\007\255\020\255\036\255\038\255\000\000\000\000\000\000\
\099\255\051\255\043\000\020\255\166\255\000\000\053\255\180\255\
\054\255\007\255\007\255\007\255\007\255\007\255\007\255\000\000\
\000\000\000\000\007\255\007\255\007\255\007\255\007\255\007\255\
\007\255\000\000\000\000\000\000\194\255\208\255\009\000\009\000\
\255\254\222\255\000\000\000\000\170\255\170\255\253\254\244\255\
\047\000\020\255\020\255\063\255\007\255\000\000\045\255\000\000\
\007\255\147\255\020\255\233\255\000\000\007\255\000\000\065\255\
\025\000\007\255\000\000\036\000\000\000"

let yyrindex = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\076\000\001\000\
\000\000\000\000\086\255\000\000\000\000\000\000\000\000\000\000\
\055\255\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\131\255\145\255\
\000\000\000\000\000\000\000\000\109\255\125\255\103\255\128\255\
\041\255\000\000\000\000\071\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\087\255\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\084\000\030\000\000\000\000\000\022\000\246\255"

let yytablesize = 318
let yytable = "\024\000\
\002\000\008\000\035\000\036\000\035\000\036\000\001\000\023\000\
\010\000\060\000\037\000\038\000\039\000\040\000\041\000\045\000\
\046\000\047\000\048\000\049\000\050\000\010\000\061\000\011\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\014\000\
\015\000\016\000\017\000\018\000\012\000\026\000\013\000\027\000\
\025\000\024\000\032\000\024\000\014\000\015\000\016\000\017\000\
\018\000\033\000\066\000\024\000\031\000\042\000\068\000\017\000\
\024\000\017\000\044\000\073\000\017\000\017\000\067\000\076\000\
\024\000\017\000\017\000\017\000\017\000\017\000\017\000\029\000\
\065\000\029\000\074\000\008\000\029\000\029\000\017\000\063\000\
\064\000\029\000\029\000\029\000\029\000\029\000\029\000\030\000\
\071\000\030\000\008\000\022\000\030\000\030\000\029\000\000\000\
\000\000\030\000\030\000\030\000\030\000\030\000\030\000\022\000\
\000\000\022\000\028\000\029\000\030\000\020\000\030\000\020\000\
\000\000\022\000\022\000\022\000\022\000\022\000\022\000\020\000\
\020\000\020\000\000\000\020\000\020\000\021\000\022\000\021\000\
\023\000\000\000\023\000\025\000\020\000\025\000\000\000\021\000\
\021\000\021\000\023\000\021\000\021\000\025\000\023\000\023\000\
\000\000\026\000\000\000\026\000\021\000\000\000\000\000\023\000\
\035\000\036\000\025\000\026\000\000\000\069\000\037\000\038\000\
\039\000\040\000\041\000\003\000\004\000\005\000\034\000\000\000\
\026\000\000\000\070\000\035\000\036\000\000\000\000\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\043\000\039\000\
\000\000\035\000\036\000\000\000\000\000\000\000\000\000\037\000\
\038\000\039\000\040\000\041\000\058\000\000\000\000\000\035\000\
\036\000\000\000\000\000\000\000\000\000\037\000\038\000\039\000\
\040\000\041\000\059\000\000\000\000\000\035\000\036\000\000\000\
\000\000\000\000\000\000\037\000\038\000\039\000\040\000\041\000\
\062\000\000\000\000\000\035\000\036\000\000\000\000\000\000\000\
\000\000\037\000\038\000\039\000\040\000\041\000\035\000\036\000\
\000\000\000\000\000\000\072\000\037\000\038\000\039\000\040\000\
\041\000\035\000\036\000\000\000\000\000\000\000\000\000\037\000\
\038\000\039\000\002\000\000\000\002\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\000\036\000\
\000\000\002\000\000\000\002\000\037\000\038\000\039\000\040\000\
\041\000\002\000\002\000\002\000\002\000\002\000\035\000\036\000\
\000\000\000\000\000\000\075\000\037\000\038\000\039\000\040\000\
\041\000\035\000\036\000\000\000\000\000\000\000\077\000\037\000\
\038\000\039\000\040\000\041\000\035\000\036\000\000\000\000\000\
\000\000\000\000\037\000\038\000\039\000\040\000"

let yycheck = "\010\000\
\000\000\000\000\006\001\007\001\006\001\007\001\001\000\028\001\
\002\001\011\001\012\001\013\001\014\001\015\001\016\001\026\000\
\027\000\028\000\029\000\030\000\031\000\002\001\024\001\004\001\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\025\001\
\026\001\027\001\028\001\029\001\017\001\002\001\019\001\002\001\
\011\000\001\001\000\000\003\001\025\001\026\001\027\001\028\001\
\029\001\020\000\061\000\011\001\002\001\001\001\065\000\001\001\
\016\001\003\001\005\001\070\000\006\001\007\001\018\001\074\000\
\024\001\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\010\001\003\001\010\001\000\000\006\001\007\001\024\001\058\000\
\059\000\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\067\000\003\001\005\001\008\000\006\001\007\001\024\001\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\255\255\003\001\008\001\009\001\010\001\001\001\024\001\003\001\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\011\001\
\012\001\013\001\255\255\015\001\016\001\001\001\024\001\003\001\
\001\001\255\255\003\001\001\001\024\001\003\001\255\255\011\001\
\012\001\013\001\011\001\015\001\016\001\011\001\015\001\016\001\
\255\255\001\001\255\255\003\001\024\001\255\255\255\255\024\001\
\006\001\007\001\024\001\011\001\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\020\001\021\001\022\001\001\001\255\255\
\024\001\255\255\024\001\006\001\007\001\255\255\255\255\006\001\
\007\001\012\001\013\001\014\001\015\001\016\001\003\001\014\001\
\255\255\006\001\007\001\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\003\001\255\255\255\255\006\001\
\007\001\255\255\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\003\001\255\255\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\012\001\013\001\014\001\015\001\016\001\
\003\001\255\255\255\255\006\001\007\001\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\006\001\007\001\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\006\001\007\001\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\002\001\255\255\004\001\255\255\005\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\007\001\
\255\255\017\001\255\255\019\001\012\001\013\001\014\001\015\001\
\016\001\025\001\026\001\027\001\028\001\029\001\006\001\007\001\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\006\001\007\001\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\006\001\007\001\255\255\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  PLUS\000\
  MINUS\000\
  ASSIGN\000\
  DASSIGN\000\
  LBRAC\000\
  RBRAC\000\
  EQ\000\
  NEQ\000\
  LT\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  RETURN\000\
  COMMA\000\
  EOF\000\
  PRINTF\000\
  "

let yynames_block = "\
  LITERAL\000\
  FLIT\000\
  BLIT\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 30 "nanocparse.mly"
                                     ( {locals=_1; body=_2} )
# 260 "nanocparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "nanocparse.mly"
                                ( []       )
# 266 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 34 "nanocparse.mly"
                                ( _1 :: _2 )
# 274 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 37 "nanocparse.mly"
                   ( (_1, _2) )
# 282 "nanocparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "nanocparse.mly"
            ( Int  )
# 288 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "nanocparse.mly"
            ( Bool )
# 294 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "nanocparse.mly"
            ( Float )
# 300 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "nanocparse.mly"
                                ( []     )
# 306 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 47 "nanocparse.mly"
                                ( _1::_2 )
# 314 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 50 "nanocparse.mly"
                                                          ( Expr _1         )
# 321 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 51 "nanocparse.mly"
                                                          ( Block _2        )
# 328 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 52 "nanocparse.mly"
                                                          ( If (_3, _5, _7) )
# 337 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 53 "nanocparse.mly"
                                                          ( While (_3,_5)   )
# 345 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 58 "nanocparse.mly"
                                  ( BoolLit _1            )
# 352 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 59 "nanocparse.mly"
                                  ( FloatLit _1           )
# 359 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 60 "nanocparse.mly"
                                  ( Literal _1            )
# 366 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "nanocparse.mly"
                                  ( Id _1                 )
# 373 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 62 "nanocparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 381 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 63 "nanocparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 389 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 64 "nanocparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 397 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 65 "nanocparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 405 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 66 "nanocparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 413 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 67 "nanocparse.mly"
                                  ( Binop (_1, And, _3)   )
# 421 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 68 "nanocparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 429 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 69 "nanocparse.mly"
                                  ( Assign (_1, _3)       )
# 437 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 70 "nanocparse.mly"
                                  ( DAssign (_1, _3)      )
# 445 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 71 "nanocparse.mly"
                                  ( _2                    )
# 452 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 72 "nanocparse.mly"
                                   ( Printf _3 )
# 459 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 73 "nanocparse.mly"
                             ( Array(_1, _3) )
# 467 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 74 "nanocparse.mly"
                                                    ( TwoDArray(_1, _3, _6) )
# 476 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 75 "nanocparse.mly"
                                                                           ( ThreeDArray(_1, _3, _6, _9) )
# 486 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 76 "nanocparse.mly"
                                               ( TwoDArray(_1, _3, _5) )
# 495 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 77 "nanocparse.mly"
                                                               ( ThreeDArray(_1, _3, _5, _7) )
# 505 "nanocparse.ml"
               : 'expr_rule))
(* Entry program_rule *)
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
let program_rule (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
