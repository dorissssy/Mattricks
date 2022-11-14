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
  | CONST
  | RETURN
  | COMMA
  | FUNCTION
  | GIVES
  | LITERAL of (int)
  | FLIT of (float)
  | BLIT of (bool)
  | ID of (string)
  | EOF
  | PRINTF
  | CONSOLE

open Parsing;;
let _ = parse_error;;
# 4 "nanocparse.mly"
open Ast
# 42 "nanocparse.ml"
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
  279 (* CONST *);
  280 (* RETURN *);
  281 (* COMMA *);
  282 (* FUNCTION *);
  283 (* GIVES *);
    0 (* EOF *);
  288 (* PRINTF *);
  289 (* CONSOLE *);
    0|]

let yytransl_block = [|
  284 (* LITERAL *);
  285 (* FLIT *);
  286 (* BLIT *);
  287 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\004\000\
\007\000\007\000\009\000\009\000\006\000\006\000\006\000\008\000\
\008\000\010\000\010\000\010\000\010\000\012\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\011\000\
\000\000\001\000\001\000\003\000\001\000\001\000\001\000\000\000\
\002\000\002\000\003\000\007\000\005\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\005\000\003\000\003\000\003\000\004\000\
\007\000\010\000\006\000\008\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\000\000\046\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\004\000\007\000\
\000\000\003\000\000\000\000\000\010\000\000\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\024\000\023\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\017\000\018\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\038\000\
\019\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\043\000\000\000\
\020\000\000\000\000\000\000\000\044\000\000\000\042\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\029\000\011\000\020\000\041\000\
\021\000\042\000\043\000\071\000"

let yysindex = "\004\000\
\206\255\000\000\000\000\000\000\000\000\239\254\000\000\033\000\
\045\255\206\255\020\255\057\255\000\000\206\255\000\000\000\000\
\028\255\000\000\039\255\063\255\000\000\028\255\041\255\000\000\
\028\255\071\255\028\255\075\255\088\255\028\255\011\255\088\255\
\084\255\085\255\011\255\000\000\000\000\000\000\053\255\066\255\
\072\255\088\255\255\255\000\000\013\000\095\255\011\255\011\255\
\085\000\050\255\011\255\011\255\011\255\000\000\000\000\000\000\
\011\255\011\255\011\255\011\255\011\255\011\255\011\255\000\000\
\000\000\027\000\041\000\000\000\011\255\085\000\028\255\085\000\
\224\255\085\000\000\000\000\000\207\255\207\255\017\255\082\255\
\096\000\088\255\088\255\085\000\011\255\092\255\011\255\096\255\
\000\000\085\000\011\255\239\255\088\255\052\000\000\000\011\255\
\000\000\093\255\063\000\011\255\000\000\074\000\000\000"

let yyrindex = "\000\000\
\123\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\123\000\000\000\000\000\000\000\123\000\000\000\000\000\
\121\255\000\000\124\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\080\255\000\000\123\255\080\255\000\000\123\255\
\000\000\000\000\000\000\000\000\000\000\000\000\119\255\000\000\
\000\000\123\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\177\255\000\000\197\255\
\000\000\200\255\000\000\000\000\174\255\181\255\158\255\042\255\
\090\255\000\000\000\000\204\255\000\000\135\255\000\000\000\000\
\000\000\209\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\151\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\252\255\241\255\000\000\099\000\240\255\000\000\232\255\
\115\000\210\255\225\255\000\000"

let yytablesize = 367
let yytable = "\045\000\
\045\000\019\000\045\000\049\000\001\000\015\000\019\000\046\000\
\026\000\018\000\045\000\028\000\031\000\012\000\028\000\066\000\
\067\000\055\000\070\000\072\000\073\000\074\000\057\000\058\000\
\045\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\013\000\069\000\035\000\088\000\089\000\084\000\036\000\037\000\
\038\000\039\000\032\000\040\000\032\000\014\000\097\000\003\000\
\004\000\005\000\016\000\031\000\032\000\090\000\085\000\092\000\
\032\000\032\000\017\000\094\000\050\000\051\000\052\000\022\000\
\099\000\023\000\032\000\025\000\102\000\003\000\004\000\005\000\
\068\000\035\000\027\000\030\000\054\000\036\000\037\000\038\000\
\039\000\005\000\040\000\005\000\005\000\047\000\048\000\057\000\
\058\000\031\000\033\000\032\000\033\000\059\000\060\000\061\000\
\005\000\053\000\005\000\065\000\033\000\091\000\100\000\005\000\
\033\000\033\000\034\000\005\000\005\000\005\000\005\000\035\000\
\005\000\093\000\033\000\036\000\037\000\038\000\039\000\026\000\
\040\000\026\000\002\000\009\000\026\000\026\000\011\000\016\000\
\044\000\026\000\026\000\026\000\026\000\026\000\026\000\040\000\
\024\000\040\000\000\000\000\000\040\000\040\000\000\000\026\000\
\000\000\040\000\040\000\040\000\040\000\040\000\040\000\041\000\
\000\000\041\000\000\000\000\000\041\000\041\000\031\000\040\000\
\031\000\041\000\041\000\041\000\041\000\041\000\041\000\000\000\
\031\000\031\000\031\000\031\000\031\000\031\000\029\000\041\000\
\029\000\034\000\000\000\034\000\000\000\030\000\031\000\030\000\
\029\000\029\000\029\000\034\000\029\000\029\000\000\000\030\000\
\030\000\030\000\000\000\030\000\030\000\037\000\029\000\037\000\
\039\000\034\000\039\000\000\000\035\000\030\000\035\000\037\000\
\000\000\036\000\039\000\036\000\057\000\058\000\035\000\000\000\
\000\000\000\000\000\000\036\000\061\000\037\000\000\000\000\000\
\039\000\003\000\004\000\005\000\035\000\057\000\058\000\006\000\
\000\000\036\000\086\000\059\000\060\000\061\000\062\000\063\000\
\000\000\000\000\000\000\000\000\057\000\058\000\000\000\000\000\
\087\000\095\000\059\000\060\000\061\000\062\000\063\000\056\000\
\000\000\000\000\000\000\000\000\057\000\058\000\000\000\096\000\
\000\000\000\000\059\000\060\000\061\000\062\000\063\000\064\000\
\000\000\000\000\057\000\058\000\000\000\000\000\000\000\000\000\
\059\000\060\000\061\000\062\000\063\000\082\000\000\000\000\000\
\057\000\058\000\000\000\000\000\000\000\000\000\059\000\060\000\
\061\000\062\000\063\000\083\000\000\000\000\000\057\000\058\000\
\000\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\063\000\057\000\058\000\000\000\000\000\000\000\098\000\059\000\
\060\000\061\000\062\000\063\000\057\000\058\000\000\000\000\000\
\000\000\101\000\059\000\060\000\061\000\062\000\063\000\057\000\
\058\000\000\000\000\000\000\000\103\000\059\000\060\000\061\000\
\062\000\063\000\057\000\058\000\000\000\000\000\000\000\000\000\
\059\000\060\000\061\000\062\000\063\000\057\000\058\000\000\000\
\000\000\000\000\000\000\059\000\060\000\061\000\062\000"

let yycheck = "\031\000\
\001\001\017\000\003\001\035\000\001\000\010\000\022\000\032\000\
\025\000\014\000\011\001\027\000\002\001\031\001\030\000\047\000\
\048\000\042\000\050\000\051\000\052\000\053\000\006\001\007\001\
\025\001\057\000\058\000\059\000\060\000\061\000\062\000\063\000\
\000\000\050\000\024\001\082\000\083\000\069\000\028\001\029\001\
\030\001\031\001\001\001\033\001\003\001\001\001\093\000\020\001\
\021\001\022\001\031\001\002\001\011\001\085\000\071\000\087\000\
\015\001\016\001\002\001\091\000\008\001\009\001\010\001\025\001\
\096\000\003\001\025\001\027\001\100\000\020\001\021\001\022\001\
\023\001\024\001\004\001\001\001\005\001\028\001\029\001\030\001\
\031\001\002\001\033\001\004\001\005\001\002\001\002\001\006\001\
\007\001\002\001\001\001\004\001\003\001\012\001\013\001\014\001\
\017\001\032\001\019\001\005\001\011\001\010\001\010\001\024\001\
\017\001\016\001\019\001\028\001\029\001\030\001\031\001\024\001\
\033\001\018\001\025\001\028\001\029\001\030\001\031\001\001\001\
\033\001\003\001\000\000\003\001\006\001\007\001\003\001\005\001\
\030\000\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\022\000\003\001\255\255\255\255\006\001\007\001\255\255\025\001\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\255\255\003\001\255\255\255\255\006\001\007\001\001\001\025\001\
\003\001\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\001\001\025\001\
\003\001\001\001\255\255\003\001\255\255\001\001\025\001\003\001\
\011\001\012\001\013\001\011\001\015\001\016\001\255\255\011\001\
\012\001\013\001\255\255\015\001\016\001\001\001\025\001\003\001\
\001\001\025\001\003\001\255\255\001\001\025\001\003\001\011\001\
\255\255\001\001\011\001\003\001\006\001\007\001\011\001\255\255\
\255\255\255\255\255\255\011\001\014\001\025\001\255\255\255\255\
\025\001\020\001\021\001\022\001\025\001\006\001\007\001\026\001\
\255\255\025\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\255\255\255\255\255\255\006\001\007\001\255\255\255\255\
\025\001\011\001\012\001\013\001\014\001\015\001\016\001\001\001\
\255\255\255\255\255\255\255\255\006\001\007\001\255\255\025\001\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\003\001\
\255\255\255\255\006\001\007\001\255\255\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\003\001\255\255\255\255\
\006\001\007\001\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\003\001\255\255\255\255\006\001\007\001\
\255\255\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\006\001\007\001\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\006\001\007\001\255\255\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\006\001\
\007\001\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\006\001\007\001\255\255\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\006\001\007\001\255\255\
\255\255\255\255\255\255\012\001\013\001\014\001\015\001"

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
  CONST\000\
  RETURN\000\
  COMMA\000\
  FUNCTION\000\
  GIVES\000\
  EOF\000\
  PRINTF\000\
  CONSOLE\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls_rule) in
    Obj.repr(
# 31 "nanocparse.mly"
                 ( _1 )
# 296 "nanocparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "nanocparse.mly"
   ( ([], []) )
# 302 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls_rule) in
    Obj.repr(
# 36 "nanocparse.mly"
                              ( ((_1 :: fst _3), snd _3) )
# 310 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls_rule) in
    Obj.repr(
# 37 "nanocparse.mly"
                         ( (fst _2, (_1 :: snd _2)) )
# 318 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "nanocparse.mly"
                                ( []       )
# 324 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 42 "nanocparse.mly"
                                     ( _1 :: _3 )
# 332 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "nanocparse.mly"
               ( (_1, _2) )
# 340 "nanocparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'typ_rule) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list_rule) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 50 "nanocparse.mly"
  (
    {
      rtyp=_7;
      fname=_2;
      formals=_4;
      locals=_9;
      body=_10
    }
  )
# 359 "nanocparse.ml"
               : 'fdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "nanocparse.mly"
    ( [] )
# 365 "nanocparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 63 "nanocparse.mly"
                 ( _1 )
# 372 "nanocparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_rule) in
    Obj.repr(
# 66 "nanocparse.mly"
               ( [_1] )
# 379 "nanocparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 67 "nanocparse.mly"
                                  ( _1::_3 )
# 387 "nanocparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "nanocparse.mly"
            ( Int  )
# 393 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "nanocparse.mly"
            ( Bool )
# 399 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "nanocparse.mly"
            ( Float )
# 405 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "nanocparse.mly"
                                ( []     )
# 411 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 77 "nanocparse.mly"
                                ( _1::_2 )
# 419 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 80 "nanocparse.mly"
                                                          ( Expr _1         )
# 426 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 81 "nanocparse.mly"
                                                          ( Block _2        )
# 433 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 82 "nanocparse.mly"
                                                          ( If (_3, _5, _7) )
# 442 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 83 "nanocparse.mly"
                                                          ( While (_3,_5)   )
# 450 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "nanocparse.mly"
            ( Const )
# 456 "nanocparse.ml"
               : 'const_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "nanocparse.mly"
                                  ( BoolLit _1            )
# 463 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 91 "nanocparse.mly"
                                  ( FloatLit _1           )
# 470 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 92 "nanocparse.mly"
                                  ( Literal _1            )
# 477 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 93 "nanocparse.mly"
                                  ( Id _1                 )
# 484 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 94 "nanocparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 492 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 95 "nanocparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 500 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 96 "nanocparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 508 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 97 "nanocparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 516 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 98 "nanocparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 524 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 99 "nanocparse.mly"
                                  ( Binop (_1, And, _3)   )
# 532 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 100 "nanocparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 540 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 101 "nanocparse.mly"
                                  ( Assign (_1, _3)       )
# 548 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 102 "nanocparse.mly"
                                  ( Assign2 (_1,_3,_4)    )
# 557 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'const_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 103 "nanocparse.mly"
                                             ( Assign3 (_1, _3, _4, _5)    )
# 567 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 104 "nanocparse.mly"
                                  ( DAssign (_1, _3)      )
# 575 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 105 "nanocparse.mly"
                                  ( _2                    )
# 582 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 106 "nanocparse.mly"
                                  ( Printf _3 )
# 589 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 107 "nanocparse.mly"
                             ( Array(_1, _3) )
# 597 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 108 "nanocparse.mly"
                                                    ( TwoDArray(_1, _3, _6) )
# 606 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 109 "nanocparse.mly"
                                                                         ( ThreeDArray(_1, _3, _6, _9) )
# 616 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 110 "nanocparse.mly"
                                             ( TwoDArray(_1, _3, _5) )
# 625 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 111 "nanocparse.mly"
                                                             ( ThreeDArray(_1, _3, _5, _7) )
# 635 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 112 "nanocparse.mly"
                                  ( Return _2 )
# 642 "nanocparse.ml"
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
