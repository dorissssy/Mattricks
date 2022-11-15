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
\007\000\007\000\009\000\009\000\006\000\006\000\006\000\010\000\
\011\000\011\000\013\000\013\000\012\000\012\000\012\000\008\000\
\008\000\015\000\015\000\015\000\015\000\016\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\011\000\
\000\000\001\000\001\000\003\000\001\000\001\000\001\000\006\000\
\000\000\003\000\000\000\002\000\001\000\003\000\001\000\000\000\
\002\000\002\000\003\000\007\000\005\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\005\000\003\000\003\000\003\000\
\004\000\007\000\010\000\006\000\008\000\002\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\013\000\014\000\015\000\000\000\056\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\004\000\007\000\
\000\000\003\000\000\000\000\000\010\000\000\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\000\032\000\031\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\047\000\
\027\000\000\000\000\000\030\000\000\000\042\000\000\000\000\000\
\000\000\000\000\000\000\035\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\000\000\000\028\000\000\000\000\000\000\000\
\016\000\000\000\053\000\000\000\051\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\029\000\011\000\020\000\041\000\
\021\000\070\000\000\000\000\000\000\000\042\000\043\000\072\000\
\000\000"

let yysindex = "\009\000\
\040\255\000\000\000\000\000\000\000\000\237\254\000\000\034\000\
\031\255\040\255\005\255\035\255\000\000\040\255\000\000\000\000\
\027\255\000\000\018\255\047\255\000\000\027\255\024\255\000\000\
\027\255\053\255\027\255\058\255\088\255\027\255\011\255\088\255\
\062\255\063\255\011\255\000\000\000\000\000\000\078\255\036\255\
\071\255\018\000\088\255\000\000\032\000\072\255\011\255\011\255\
\118\000\050\255\011\255\011\255\011\255\000\000\000\000\011\255\
\011\255\011\255\011\255\011\255\011\255\011\255\000\000\000\000\
\000\000\046\000\060\000\000\000\065\255\000\000\118\000\027\255\
\118\000\243\255\118\000\000\000\000\000\039\255\039\255\017\255\
\133\000\129\000\088\255\088\255\098\255\118\000\011\255\081\255\
\011\255\083\255\000\000\077\255\118\000\011\255\002\000\088\255\
\075\255\085\000\000\000\011\255\000\000\103\255\104\255\096\000\
\000\000\011\255\000\000\107\000\000\000"

let yyrindex = "\000\000\
\115\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\115\000\000\000\000\000\000\000\115\000\000\000\000\000\
\117\255\000\000\121\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\080\255\000\000\134\255\080\255\000\000\134\255\
\000\000\000\000\000\000\000\000\000\000\000\000\129\255\000\000\
\000\000\000\000\134\255\000\000\000\000\000\000\000\000\000\000\
\187\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\210\255\000\000\
\214\255\000\000\219\255\000\000\000\000\184\255\191\255\168\255\
\122\255\207\255\000\000\000\000\000\000\223\255\000\000\145\255\
\000\000\000\000\000\000\074\000\226\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\161\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\251\255\240\255\000\000\104\000\239\255\000\000\231\255\
\127\000\000\000\000\000\000\000\000\000\225\255\175\255\000\000\
\000\000"

let yytablesize = 403
let yytable = "\045\000\
\019\000\090\000\091\000\049\000\015\000\019\000\046\000\026\000\
\018\000\001\000\028\000\012\000\031\000\028\000\101\000\066\000\
\067\000\063\000\071\000\073\000\074\000\075\000\056\000\057\000\
\076\000\077\000\078\000\079\000\080\000\081\000\082\000\014\000\
\069\000\013\000\035\000\016\000\017\000\086\000\036\000\037\000\
\038\000\039\000\022\000\040\000\056\000\057\000\003\000\004\000\
\005\000\023\000\025\000\031\000\060\000\045\000\087\000\093\000\
\027\000\095\000\030\000\003\000\004\000\005\000\098\000\047\000\
\048\000\006\000\085\000\053\000\104\000\003\000\004\000\005\000\
\068\000\035\000\108\000\054\000\065\000\036\000\037\000\038\000\
\039\000\005\000\040\000\005\000\005\000\050\000\051\000\052\000\
\035\000\031\000\094\000\032\000\036\000\037\000\038\000\039\000\
\005\000\040\000\005\000\031\000\096\000\097\000\102\000\005\000\
\033\000\105\000\034\000\005\000\005\000\005\000\005\000\035\000\
\005\000\106\000\002\000\036\000\037\000\038\000\039\000\009\000\
\040\000\035\000\040\000\011\000\040\000\092\000\037\000\038\000\
\039\000\034\000\040\000\034\000\040\000\044\000\034\000\034\000\
\040\000\040\000\024\000\034\000\034\000\034\000\034\000\034\000\
\034\000\049\000\040\000\049\000\024\000\000\000\049\000\049\000\
\000\000\034\000\000\000\049\000\049\000\049\000\049\000\049\000\
\049\000\050\000\000\000\050\000\000\000\000\000\050\000\050\000\
\039\000\049\000\039\000\050\000\050\000\050\000\050\000\050\000\
\050\000\000\000\039\000\039\000\039\000\039\000\039\000\039\000\
\037\000\050\000\037\000\054\000\000\000\054\000\000\000\038\000\
\039\000\038\000\037\000\037\000\037\000\054\000\037\000\037\000\
\000\000\038\000\038\000\038\000\000\000\038\000\038\000\041\000\
\037\000\041\000\043\000\054\000\043\000\000\000\046\000\038\000\
\046\000\041\000\000\000\048\000\043\000\048\000\041\000\044\000\
\046\000\044\000\045\000\000\000\045\000\048\000\000\000\041\000\
\000\000\044\000\043\000\000\000\045\000\000\000\046\000\000\000\
\000\000\000\000\000\000\048\000\000\000\000\000\000\000\044\000\
\056\000\057\000\045\000\000\000\000\000\088\000\058\000\059\000\
\060\000\061\000\062\000\000\000\000\000\000\000\000\000\056\000\
\057\000\000\000\000\000\089\000\099\000\058\000\059\000\060\000\
\061\000\062\000\055\000\000\000\000\000\000\000\000\000\056\000\
\057\000\000\000\100\000\000\000\000\000\058\000\059\000\060\000\
\061\000\062\000\064\000\000\000\000\000\056\000\057\000\000\000\
\000\000\000\000\000\000\058\000\059\000\060\000\061\000\062\000\
\083\000\000\000\000\000\056\000\057\000\000\000\000\000\000\000\
\000\000\058\000\059\000\060\000\061\000\062\000\084\000\000\000\
\000\000\056\000\057\000\000\000\000\000\000\000\000\000\058\000\
\059\000\060\000\061\000\062\000\033\000\000\000\000\000\033\000\
\033\000\000\000\000\000\000\000\000\000\033\000\033\000\033\000\
\033\000\033\000\056\000\057\000\000\000\000\000\000\000\103\000\
\058\000\059\000\060\000\061\000\062\000\056\000\057\000\000\000\
\000\000\000\000\107\000\058\000\059\000\060\000\061\000\062\000\
\056\000\057\000\000\000\000\000\000\000\109\000\058\000\059\000\
\060\000\061\000\062\000\056\000\057\000\000\000\000\000\000\000\
\000\000\058\000\059\000\060\000\061\000\062\000\056\000\057\000\
\000\000\000\000\056\000\057\000\058\000\059\000\060\000\061\000\
\058\000\059\000\060\000"

let yycheck = "\031\000\
\017\000\083\000\084\000\035\000\010\000\022\000\032\000\025\000\
\014\000\001\000\027\000\031\001\002\001\030\000\096\000\047\000\
\048\000\043\000\050\000\051\000\052\000\053\000\006\001\007\001\
\056\000\057\000\058\000\059\000\060\000\061\000\062\000\001\001\
\050\000\000\000\024\001\031\001\002\001\069\000\028\001\029\001\
\030\001\031\001\025\001\033\001\006\001\007\001\020\001\021\001\
\022\001\003\001\027\001\002\001\014\001\085\000\072\000\087\000\
\004\001\089\000\001\001\020\001\021\001\022\001\094\000\002\001\
\002\001\026\001\002\001\032\001\100\000\020\001\021\001\022\001\
\023\001\024\001\106\000\005\001\005\001\028\001\029\001\030\001\
\031\001\002\001\033\001\004\001\005\001\008\001\009\001\010\001\
\024\001\002\001\010\001\004\001\028\001\029\001\030\001\031\001\
\017\001\033\001\019\001\002\001\018\001\025\001\028\001\024\001\
\017\001\003\001\019\001\028\001\029\001\030\001\031\001\024\001\
\033\001\010\001\000\000\028\001\029\001\030\001\031\001\003\001\
\033\001\024\001\001\001\003\001\003\001\028\001\029\001\030\001\
\031\001\001\001\033\001\003\001\011\001\030\000\006\001\007\001\
\015\001\016\001\005\001\011\001\012\001\013\001\014\001\015\001\
\016\001\001\001\025\001\003\001\022\000\255\255\006\001\007\001\
\255\255\025\001\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\001\001\025\001\003\001\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\001\001\025\001\003\001\001\001\255\255\003\001\255\255\001\001\
\025\001\003\001\011\001\012\001\013\001\011\001\015\001\016\001\
\255\255\011\001\012\001\013\001\255\255\015\001\016\001\001\001\
\025\001\003\001\001\001\025\001\003\001\255\255\001\001\025\001\
\003\001\011\001\255\255\001\001\011\001\003\001\016\001\001\001\
\011\001\003\001\001\001\255\255\003\001\011\001\255\255\025\001\
\255\255\011\001\025\001\255\255\011\001\255\255\025\001\255\255\
\255\255\255\255\255\255\025\001\255\255\255\255\255\255\025\001\
\006\001\007\001\025\001\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\255\255\255\255\006\001\
\007\001\255\255\255\255\025\001\011\001\012\001\013\001\014\001\
\015\001\016\001\001\001\255\255\255\255\255\255\255\255\006\001\
\007\001\255\255\025\001\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\003\001\255\255\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\012\001\013\001\014\001\015\001\016\001\
\003\001\255\255\255\255\006\001\007\001\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\003\001\255\255\
\255\255\006\001\007\001\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\003\001\255\255\255\255\006\001\
\007\001\255\255\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\006\001\007\001\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\006\001\007\001\255\255\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\006\001\007\001\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\006\001\007\001\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\006\001\007\001\
\255\255\255\255\006\001\007\001\012\001\013\001\014\001\015\001\
\012\001\013\001\014\001"

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
# 313 "nanocparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "nanocparse.mly"
   ( ([], []) )
# 319 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls_rule) in
    Obj.repr(
# 36 "nanocparse.mly"
                              ( ((_1 :: fst _3), snd _3) )
# 327 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls_rule) in
    Obj.repr(
# 37 "nanocparse.mly"
                         ( (fst _2, (_1 :: snd _2)) )
# 335 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "nanocparse.mly"
                                ( []       )
# 341 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 42 "nanocparse.mly"
                                     ( _1 :: _3 )
# 349 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "nanocparse.mly"
               ( (_1, _2) )
# 357 "nanocparse.ml"
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
# 376 "nanocparse.ml"
               : 'fdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "nanocparse.mly"
    ( [] )
# 382 "nanocparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 63 "nanocparse.mly"
                 ( _1 )
# 389 "nanocparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_rule) in
    Obj.repr(
# 66 "nanocparse.mly"
               ( [_1] )
# 396 "nanocparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 67 "nanocparse.mly"
                                  ( _1::_3 )
# 404 "nanocparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "nanocparse.mly"
            ( Int  )
# 410 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "nanocparse.mly"
            ( Bool )
# 416 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "nanocparse.mly"
            ( Float )
# 422 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'typ_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 75 "nanocparse.mly"
                                               ( Mtype (_1, _3, _5) )
# 431 "nanocparse.ml"
               : 'mat_typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "nanocparse.mly"
                                ( []     )
# 437 "nanocparse.ml"
               : 'mat_array_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mat_array_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mat_array_list_rule) in
    Obj.repr(
# 79 "nanocparse.mly"
                                                ( _1::_3 )
# 445 "nanocparse.ml"
               : 'mat_array_list_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "nanocparse.mly"
                                ( []     )
# 451 "nanocparse.ml"
               : 'expr_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list_rule) in
    Obj.repr(
# 83 "nanocparse.mly"
                                ( _1::_2 )
# 459 "nanocparse.ml"
               : 'expr_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 87 "nanocparse.mly"
                                                       ( MatValue _1 )
# 466 "nanocparse.ml"
               : 'mat_array_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list_rule) in
    Obj.repr(
# 88 "nanocparse.mly"
                                                       ( MatArray _2 )
# 473 "nanocparse.ml"
               : 'mat_array_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mat_array_list_rule) in
    Obj.repr(
# 89 "nanocparse.mly"
                                         ( Mat _1 )
# 480 "nanocparse.ml"
               : 'mat_array_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "nanocparse.mly"
                                ( []     )
# 486 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 94 "nanocparse.mly"
                                ( _1::_2 )
# 494 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 97 "nanocparse.mly"
                                                          ( Expr _1         )
# 501 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 98 "nanocparse.mly"
                                                          ( Block _2        )
# 508 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 99 "nanocparse.mly"
                                                          ( If (_3, _5, _7) )
# 517 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 100 "nanocparse.mly"
                                                          ( While (_3,_5)   )
# 525 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "nanocparse.mly"
            ( Const )
# 531 "nanocparse.ml"
               : 'const_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 107 "nanocparse.mly"
                                  ( BoolLit _1            )
# 538 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 108 "nanocparse.mly"
                                  ( FloatLit _1           )
# 545 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 109 "nanocparse.mly"
                                  ( Literal _1            )
# 552 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "nanocparse.mly"
                                  ( Id _1                 )
# 559 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 111 "nanocparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 567 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 112 "nanocparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 575 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 113 "nanocparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 583 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 114 "nanocparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 591 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 115 "nanocparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 599 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 116 "nanocparse.mly"
                                  ( Binop (_1, And, _3)   )
# 607 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 117 "nanocparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 615 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mat_typ_rule) in
    Obj.repr(
# 118 "nanocparse.mly"
                                  ( AssignMatTyp (_1, _3) )
# 623 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 119 "nanocparse.mly"
                                  ( Assign (_1, _3)       )
# 631 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 120 "nanocparse.mly"
                                  ( Assign2 (_1,_3,_4)    )
# 640 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'const_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 121 "nanocparse.mly"
                                             ( Assign3 (_1, _3, _4, _5)    )
# 650 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 122 "nanocparse.mly"
                                  ( DAssign (_1, _3)      )
# 658 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 123 "nanocparse.mly"
                                  ( _2                    )
# 665 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 124 "nanocparse.mly"
                                  ( Printf _3 )
# 672 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 125 "nanocparse.mly"
                             ( Array(_1, _3) )
# 680 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 126 "nanocparse.mly"
                                                    ( TwoDArray(_1, _3, _6) )
# 689 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 127 "nanocparse.mly"
                                                                         ( ThreeDArray(_1, _3, _6, _9) )
# 699 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 128 "nanocparse.mly"
                                             ( TwoDArray(_1, _3, _5) )
# 708 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 129 "nanocparse.mly"
                                                             ( ThreeDArray(_1, _3, _5, _7) )
# 718 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 130 "nanocparse.mly"
                                  ( Return _2 )
# 725 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'mat_typ_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'mat_array_rule) in
    Obj.repr(
# 133 "nanocparse.mly"
                                                        ( AssignMat (_1, _3, _5) )
# 734 "nanocparse.ml"
               : 'mat_rule))
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
