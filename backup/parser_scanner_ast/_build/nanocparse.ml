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
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\003\000\
\003\000\004\000\008\000\008\000\010\000\010\000\006\000\006\000\
\006\000\006\000\006\000\006\000\009\000\009\000\011\000\011\000\
\011\000\011\000\011\000\012\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\013\000\013\000\014\000\014\000\
\000\000"

let yylen = "\002\000\
\002\000\000\000\003\000\002\000\000\000\003\000\002\000\003\000\
\004\000\011\000\000\000\001\000\001\000\003\000\001\000\001\000\
\001\000\006\000\004\000\008\000\000\000\002\000\002\000\003\000\
\007\000\005\000\003\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\005\000\003\000\003\000\005\000\003\000\004\000\007\000\
\010\000\006\000\008\000\004\000\000\000\001\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\015\000\016\000\017\000\000\000\000\000\057\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\004\000\000\000\007\000\000\000\000\000\003\000\000\000\000\000\
\000\000\012\000\000\000\031\000\030\000\029\000\000\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\044\000\
\000\000\000\000\054\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\034\000\000\000\000\000\000\000\000\000\
\000\000\018\000\000\000\000\000\000\000\052\000\000\000\000\000\
\000\000\000\000\000\000\000\000\056\000\045\000\000\000\000\000\
\000\000\020\000\000\000\000\000\000\000\050\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\023\000\010\000\
\022\000\000\000\051\000\024\000\000\000\000\000\027\000\000\000\
\000\000\000\000\049\000\000\000\026\000\000\000\025\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\092\000\012\000\101\000\025\000\
\102\000\026\000\103\000\063\000\058\000\059\000"

let yysindex = "\004\000\
\081\255\000\000\000\000\000\000\000\000\237\254\005\255\000\000\
\039\000\015\255\081\255\006\255\056\255\254\255\000\000\081\255\
\000\000\020\255\000\000\088\255\066\255\000\000\035\255\025\255\
\060\255\000\000\096\255\000\000\000\000\000\000\001\255\043\255\
\148\000\000\000\052\255\088\255\058\255\103\255\035\255\076\000\
\103\255\013\255\103\255\103\255\103\255\103\255\103\255\103\255\
\103\255\103\255\103\255\103\255\048\255\000\000\254\255\000\000\
\030\000\097\255\000\000\000\000\066\255\148\000\254\255\148\000\
\001\000\148\000\000\000\000\000\107\255\107\255\116\255\170\000\
\058\000\000\000\062\255\156\255\103\255\000\000\245\255\066\255\
\108\255\103\255\136\255\088\255\000\000\000\000\148\000\103\255\
\016\000\000\000\143\255\053\255\115\000\000\000\103\255\088\255\
\053\255\152\255\153\255\103\255\046\000\163\255\053\255\161\255\
\126\000\000\000\165\255\103\255\103\255\062\000\000\000\000\000\
\000\000\103\255\000\000\000\000\090\000\104\000\000\000\137\000\
\053\255\053\255\000\000\158\255\000\000\053\255\000\000"

let yyrindex = "\000\000\
\174\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\174\000\000\000\000\000\000\000\000\000\174\000\
\000\000\000\000\000\000\181\255\046\255\000\000\000\000\183\255\
\000\000\000\000\000\000\000\000\000\000\000\000\134\255\000\000\
\240\255\000\000\000\000\000\000\000\000\000\000\159\000\000\000\
\184\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\196\255\000\000\000\000\000\000\150\255\051\255\000\000\208\255\
\000\000\231\255\000\000\000\000\205\255\212\255\189\255\127\255\
\228\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\166\255\000\000\000\000\087\255\000\000\000\000\237\255\000\000\
\000\000\000\000\000\000\207\255\000\000\000\000\000\000\087\255\
\207\255\000\000\000\000\000\000\000\000\000\000\207\255\182\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\003\000\238\255\000\000\114\000\246\255\235\255\000\000\
\160\255\186\000\199\255\000\000\000\000\149\000"

let yytablesize = 440
let yytable = "\033\000\
\107\000\024\000\041\000\021\000\001\000\040\000\113\000\018\000\
\042\000\043\000\044\000\013\000\014\000\017\000\038\000\016\000\
\040\000\024\000\022\000\057\000\062\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\061\000\
\003\000\004\000\005\000\060\000\019\000\034\000\015\000\079\000\
\028\000\029\000\030\000\031\000\076\000\032\000\008\000\023\000\
\008\000\036\000\074\000\041\000\080\000\041\000\038\000\057\000\
\097\000\020\000\087\000\035\000\089\000\041\000\037\000\124\000\
\125\000\091\000\093\000\027\000\127\000\098\000\008\000\099\000\
\075\000\105\000\045\000\041\000\100\000\091\000\110\000\053\000\
\028\000\029\000\030\000\031\000\055\000\032\000\117\000\118\000\
\005\000\083\000\005\000\005\000\120\000\028\000\029\000\030\000\
\031\000\038\000\032\000\078\000\003\000\004\000\005\000\005\000\
\038\000\005\000\006\000\003\000\004\000\005\000\005\000\007\000\
\046\000\047\000\005\000\005\000\005\000\088\000\007\000\005\000\
\050\000\046\000\047\000\039\000\029\000\030\000\031\000\038\000\
\032\000\038\000\028\000\029\000\030\000\031\000\032\000\032\000\
\032\000\038\000\090\000\032\000\032\000\038\000\038\000\096\000\
\032\000\032\000\032\000\032\000\032\000\032\000\040\000\038\000\
\040\000\108\000\109\000\040\000\040\000\018\000\032\000\084\000\
\040\000\040\000\040\000\040\000\040\000\040\000\047\000\112\000\
\047\000\116\000\114\000\047\000\047\000\002\000\040\000\126\000\
\047\000\047\000\047\000\047\000\047\000\047\000\048\000\011\000\
\048\000\013\000\053\000\048\000\048\000\037\000\047\000\037\000\
\048\000\048\000\048\000\048\000\048\000\048\000\055\000\037\000\
\037\000\037\000\037\000\037\000\037\000\035\000\048\000\035\000\
\043\000\106\000\043\000\021\000\036\000\037\000\036\000\035\000\
\035\000\035\000\043\000\035\000\035\000\054\000\036\000\036\000\
\036\000\085\000\036\000\036\000\039\000\035\000\039\000\046\000\
\043\000\046\000\000\000\000\000\036\000\042\000\039\000\042\000\
\009\000\046\000\009\000\039\000\000\000\086\000\000\000\042\000\
\000\000\000\000\046\000\047\000\039\000\000\000\000\000\046\000\
\048\000\049\000\050\000\051\000\052\000\042\000\046\000\047\000\
\009\000\000\000\000\000\081\000\048\000\049\000\050\000\051\000\
\052\000\003\000\004\000\005\000\000\000\046\000\047\000\000\000\
\000\000\082\000\094\000\048\000\049\000\050\000\051\000\052\000\
\000\000\000\000\000\000\046\000\047\000\000\000\000\000\000\000\
\095\000\048\000\049\000\050\000\051\000\052\000\111\000\000\000\
\000\000\000\000\000\000\046\000\047\000\000\000\077\000\000\000\
\000\000\048\000\049\000\050\000\051\000\052\000\119\000\046\000\
\047\000\000\000\000\000\046\000\047\000\048\000\049\000\050\000\
\051\000\048\000\049\000\050\000\051\000\052\000\056\000\000\000\
\000\000\046\000\047\000\000\000\000\000\000\000\000\000\048\000\
\049\000\050\000\051\000\052\000\121\000\000\000\000\000\046\000\
\047\000\000\000\000\000\000\000\000\000\048\000\049\000\050\000\
\051\000\052\000\122\000\000\000\000\000\046\000\047\000\000\000\
\000\000\000\000\000\000\048\000\049\000\050\000\051\000\052\000\
\046\000\047\000\000\000\000\000\000\000\104\000\048\000\049\000\
\050\000\051\000\052\000\046\000\047\000\000\000\000\000\000\000\
\115\000\048\000\049\000\050\000\051\000\052\000\046\000\047\000\
\000\000\000\000\000\000\123\000\048\000\049\000\050\000\051\000\
\052\000\046\000\047\000\000\000\000\000\000\000\000\000\048\000\
\049\000\050\000\051\000\052\000\031\000\031\000\000\000\000\000\
\000\000\000\000\031\000\031\000\031\000\031\000\031\000\046\000\
\047\000\000\000\000\000\000\000\000\000\048\000\049\000\050\000"

let yycheck = "\021\000\
\097\000\020\000\002\001\014\000\001\000\027\000\103\000\002\001\
\008\001\009\001\010\001\031\001\008\001\011\000\002\001\001\001\
\038\000\036\000\016\000\041\000\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\042\000\
\020\001\021\001\022\001\023\001\031\001\003\001\000\000\061\000\
\028\001\029\001\030\001\031\001\055\000\033\001\001\001\028\001\
\003\001\025\001\003\001\001\001\063\000\003\001\002\001\077\000\
\004\001\002\001\080\000\025\001\082\000\011\001\003\001\121\000\
\122\000\084\000\088\000\002\001\126\000\017\001\025\001\019\001\
\025\001\095\000\032\001\025\001\024\001\096\000\100\000\028\001\
\028\001\029\001\030\001\031\001\027\001\033\001\108\000\109\000\
\002\001\028\001\004\001\005\001\114\000\028\001\029\001\030\001\
\031\001\002\001\033\001\003\001\020\001\021\001\022\001\017\001\
\002\001\019\001\026\001\020\001\021\001\022\001\024\001\031\001\
\006\001\007\001\028\001\029\001\030\001\010\001\031\001\033\001\
\014\001\006\001\007\001\028\001\029\001\030\001\031\001\001\001\
\033\001\003\001\028\001\029\001\030\001\031\001\001\001\033\001\
\003\001\011\001\003\001\006\001\007\001\015\001\016\001\001\001\
\011\001\012\001\013\001\014\001\015\001\016\001\001\001\025\001\
\003\001\002\001\002\001\006\001\007\001\002\001\025\001\004\001\
\011\001\012\001\013\001\014\001\015\001\016\001\001\001\005\001\
\003\001\005\001\010\001\006\001\007\001\000\000\025\001\018\001\
\011\001\012\001\013\001\014\001\015\001\016\001\001\001\003\001\
\003\001\003\001\003\001\006\001\007\001\001\001\025\001\003\001\
\011\001\012\001\013\001\014\001\015\001\016\001\003\001\011\001\
\012\001\013\001\014\001\015\001\016\001\001\001\025\001\003\001\
\001\001\096\000\003\001\005\001\001\001\025\001\003\001\011\001\
\012\001\013\001\011\001\015\001\016\001\036\000\011\001\012\001\
\013\001\077\000\015\001\016\001\001\001\025\001\003\001\001\001\
\025\001\003\001\255\255\255\255\025\001\001\001\011\001\003\001\
\001\001\011\001\003\001\016\001\255\255\001\001\255\255\011\001\
\255\255\255\255\006\001\007\001\025\001\255\255\255\255\025\001\
\012\001\013\001\014\001\015\001\016\001\025\001\006\001\007\001\
\025\001\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\020\001\021\001\022\001\255\255\006\001\007\001\255\255\
\255\255\025\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\255\255\255\255\006\001\007\001\255\255\255\255\255\255\
\025\001\012\001\013\001\014\001\015\001\016\001\001\001\255\255\
\255\255\255\255\255\255\006\001\007\001\255\255\025\001\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\001\001\006\001\
\007\001\255\255\255\255\006\001\007\001\012\001\013\001\014\001\
\015\001\012\001\013\001\014\001\015\001\016\001\003\001\255\255\
\255\255\006\001\007\001\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\003\001\255\255\255\255\006\001\
\007\001\255\255\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\003\001\255\255\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\012\001\013\001\014\001\015\001\016\001\
\006\001\007\001\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\006\001\007\001\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\006\001\007\001\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\006\001\007\001\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\006\001\007\001\255\255\255\255\
\255\255\255\255\012\001\013\001\014\001\015\001\016\001\006\001\
\007\001\255\255\255\255\255\255\255\255\012\001\013\001\014\001"

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
# 327 "nanocparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "nanocparse.mly"
   ( ([], []) )
# 333 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'decls_rule) in
    Obj.repr(
# 36 "nanocparse.mly"
                              ( ((_1 :: fst _3), snd _3) )
# 341 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls_rule) in
    Obj.repr(
# 37 "nanocparse.mly"
                         ( (fst _2, (_1 :: snd _2)) )
# 349 "nanocparse.ml"
               : 'decls_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "nanocparse.mly"
                                ( []       )
# 355 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 42 "nanocparse.mly"
                                     ( _1 :: _3 )
# 363 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "nanocparse.mly"
               ( (_1, _2) )
# 371 "nanocparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ_rule) in
    Obj.repr(
# 46 "nanocparse.mly"
                       ( (_3, _1) )
# 379 "nanocparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 47 "nanocparse.mly"
                                 ( (_3, _1) )
# 388 "nanocparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'typ_rule) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list_rule) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 52 "nanocparse.mly"
  (
    {
      rtyp=_7;
      fname=_2;
      formals=_4;
      locals=_9;
      body=_10
    }
  )
# 407 "nanocparse.ml"
               : 'fdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "nanocparse.mly"
    ( [] )
# 413 "nanocparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 65 "nanocparse.mly"
                 ( _1 )
# 420 "nanocparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_rule) in
    Obj.repr(
# 68 "nanocparse.mly"
               ( [_1] )
# 427 "nanocparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 69 "nanocparse.mly"
                                  ( _1::_3 )
# 435 "nanocparse.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "nanocparse.mly"
            ( Int  )
# 441 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "nanocparse.mly"
            ( Bool )
# 447 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "nanocparse.mly"
            ( Float )
# 453 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'typ_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 75 "nanocparse.mly"
                                                 ( Mtype (_1, _3, _5) )
# 462 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 76 "nanocparse.mly"
                                   ( Vtype (_1, _3) )
# 470 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'typ_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 77 "nanocparse.mly"
                                                               ( Ttype (_1, _3, _5, _7) )
# 480 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "nanocparse.mly"
                                ( []     )
# 486 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 84 "nanocparse.mly"
                                ( _1::_2 )
# 494 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 87 "nanocparse.mly"
                                                          ( Expr _1         )
# 501 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 88 "nanocparse.mly"
                                                          ( Block _2        )
# 508 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 89 "nanocparse.mly"
                                                          ( If (_3, _5, _7) )
# 517 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 90 "nanocparse.mly"
                                                          ( While (_3,_5)   )
# 525 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 91 "nanocparse.mly"
                                                 ( Return _2      )
# 532 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "nanocparse.mly"
            ( Const )
# 538 "nanocparse.ml"
               : 'const_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 98 "nanocparse.mly"
                                  ( BoolLit _1            )
# 545 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 99 "nanocparse.mly"
                                  ( FloatLit _1           )
# 552 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 100 "nanocparse.mly"
                                  ( Literal _1            )
# 559 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 101 "nanocparse.mly"
                                  ( Id _1                 )
# 566 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 102 "nanocparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 574 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 103 "nanocparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 582 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 104 "nanocparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 590 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 105 "nanocparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 598 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 106 "nanocparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 606 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 107 "nanocparse.mly"
                                  ( Binop (_1, And, _3)   )
# 614 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 108 "nanocparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 622 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ_rule) in
    Obj.repr(
# 109 "nanocparse.mly"
                              ( AssignMat (_1, _3)    )
# 630 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 110 "nanocparse.mly"
                                  ( Assign (_1, _3)       )
# 638 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'const_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'typ_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 111 "nanocparse.mly"
                                             ( Assign3 (_1, _3, _4, _5)    )
# 648 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 112 "nanocparse.mly"
                                  ( DAssign (_1, _3)      )
# 656 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 113 "nanocparse.mly"
                                  ( _2                    )
# 663 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'typ_rule) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 114 "nanocparse.mly"
                                              ( BindAssign (_3, _1, _4) )
# 672 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 115 "nanocparse.mly"
                                  ( Printf _3 )
# 679 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 116 "nanocparse.mly"
                             ( ArrayAccess(_1, _3) )
# 687 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 117 "nanocparse.mly"
                                                    ( TwoDArrayAccess(_1, _3, _6) )
# 696 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 7 : 'expr_rule) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 118 "nanocparse.mly"
                                                                         ( ThreeDArrayAccess(_1, _3, _6, _9) )
# 706 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 119 "nanocparse.mly"
                                             ( TwoDArrayAccess(_1, _3, _5) )
# 715 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'expr_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 120 "nanocparse.mly"
                                                             ( ThreeDArrayAccess(_1, _3, _5, _7) )
# 725 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 121 "nanocparse.mly"
                              ( Call (_1, _3)  )
# 733 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "nanocparse.mly"
              ( [] )
# 739 "nanocparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 126 "nanocparse.mly"
         ( _1 )
# 746 "nanocparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 129 "nanocparse.mly"
             ( [_1] )
# 753 "nanocparse.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 130 "nanocparse.mly"
                         ( _1::_3 )
# 761 "nanocparse.ml"
               : 'args))
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
