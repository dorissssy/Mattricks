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

open Parsing;;
let _ = parse_error;;
# 4 "nanocparse.mly"
open Ast
# 35 "nanocparse.ml"
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
  266 (* EQ *);
  267 (* NEQ *);
  268 (* LT *);
  269 (* AND *);
  270 (* OR *);
  271 (* IF *);
  272 (* ELSE *);
  273 (* WHILE *);
  274 (* INT *);
  275 (* BOOL *);
  276 (* FLOAT *);
  277 (* RETURN *);
  278 (* COMMA *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  279 (* LITERAL *);
  280 (* FLIT *);
  281 (* BLIT *);
  282 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\005\000\005\000\005\000\003\000\
\003\000\006\000\006\000\006\000\006\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\003\000\001\000\001\000\001\000\000\000\
\002\000\002\000\003\000\007\000\005\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\005\000\006\000\007\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\015\000\014\000\
\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\009\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\027\000\
\011\000\000\000\000\000\000\000\000\000\018\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\012\000"

let yydgoto = "\002\000\
\006\000\007\000\018\000\008\000\009\000\019\000\020\000"

let yysindex = "\030\000\
\041\255\000\000\000\000\000\000\000\000\000\000\017\255\041\255\
\013\255\012\255\017\255\046\255\060\255\000\000\000\000\000\000\
\004\255\065\000\017\255\057\255\000\000\065\255\102\255\068\255\
\012\255\012\255\012\255\012\255\000\000\000\000\000\000\012\255\
\012\255\012\255\012\255\012\255\012\255\012\255\000\000\000\000\
\000\000\114\255\126\255\135\255\135\255\000\000\000\000\254\254\
\254\254\045\255\152\255\144\255\017\255\017\255\059\255\000\000\
\017\255\000\000"

let yyrindex = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\076\000\001\000\
\000\000\000\000\073\255\000\000\000\000\000\000\000\000\000\000\
\043\255\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\255\044\255\000\000\000\000\076\255\
\090\255\071\255\093\255\006\255\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\072\000\248\255\000\000\000\000\232\255\246\255"

let yytablesize = 283
let yytable = "\023\000\
\002\000\008\000\024\000\032\000\033\000\025\000\024\000\025\000\
\024\000\036\000\030\000\027\000\028\000\010\000\042\000\043\000\
\044\000\045\000\010\000\024\000\011\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\055\000\056\000\001\000\012\000\
\058\000\013\000\014\000\015\000\016\000\017\000\022\000\014\000\
\015\000\016\000\017\000\017\000\026\000\017\000\026\000\025\000\
\017\000\017\000\032\000\033\000\017\000\017\000\017\000\017\000\
\017\000\031\000\003\000\004\000\005\000\026\000\032\000\033\000\
\029\000\039\000\034\000\035\000\036\000\037\000\038\000\022\000\
\041\000\022\000\057\000\008\000\020\000\008\000\020\000\021\000\
\022\000\022\000\022\000\022\000\022\000\020\000\020\000\000\000\
\020\000\020\000\021\000\000\000\021\000\023\000\000\000\023\000\
\000\000\000\000\000\000\021\000\021\000\000\000\021\000\021\000\
\040\000\023\000\023\000\032\000\033\000\000\000\000\000\034\000\
\035\000\036\000\037\000\038\000\053\000\000\000\000\000\032\000\
\033\000\000\000\000\000\034\000\035\000\036\000\037\000\038\000\
\054\000\000\000\000\000\032\000\033\000\000\000\000\000\034\000\
\035\000\036\000\037\000\038\000\032\000\033\000\000\000\000\000\
\034\000\035\000\036\000\037\000\038\000\032\000\033\000\000\000\
\000\000\034\000\035\000\036\000\037\000\032\000\033\000\000\000\
\000\000\034\000\035\000\036\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\002\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\002\000\
\002\000\002\000\002\000"

let yycheck = "\010\000\
\000\000\000\000\011\000\006\001\007\001\001\001\001\001\003\001\
\003\001\012\001\019\000\008\001\009\001\002\001\025\000\026\000\
\027\000\028\000\002\001\014\001\004\001\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\053\000\054\000\001\000\015\001\
\057\000\017\001\023\001\024\001\025\001\026\001\026\001\023\001\
\024\001\025\001\026\001\001\001\001\001\003\001\003\001\002\001\
\006\001\007\001\006\001\007\001\010\001\011\001\012\001\013\001\
\014\001\001\001\018\001\019\001\020\001\002\001\006\001\007\001\
\000\000\001\001\010\001\011\001\012\001\013\001\014\001\001\001\
\005\001\003\001\016\001\000\000\001\001\005\001\003\001\008\000\
\010\001\011\001\012\001\013\001\014\001\010\001\011\001\255\255\
\013\001\014\001\001\001\255\255\003\001\001\001\255\255\003\001\
\255\255\255\255\255\255\010\001\011\001\255\255\013\001\014\001\
\003\001\013\001\014\001\006\001\007\001\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\003\001\255\255\255\255\006\001\
\007\001\255\255\255\255\010\001\011\001\012\001\013\001\014\001\
\003\001\255\255\255\255\006\001\007\001\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\006\001\007\001\255\255\255\255\
\010\001\011\001\012\001\013\001\014\001\006\001\007\001\255\255\
\255\255\010\001\011\001\012\001\013\001\006\001\007\001\255\255\
\255\255\010\001\011\001\012\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\004\001\255\255\005\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\255\255\017\001\255\255\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\026\001"

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
# 235 "nanocparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "nanocparse.mly"
                                ( []       )
# 241 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list_rule) in
    Obj.repr(
# 34 "nanocparse.mly"
                                ( _1 :: _2 )
# 249 "nanocparse.ml"
               : 'vdecl_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 37 "nanocparse.mly"
                   ( (_1, _2) )
# 257 "nanocparse.ml"
               : 'vdecl_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "nanocparse.mly"
            ( Int  )
# 263 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "nanocparse.mly"
            ( Bool )
# 269 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "nanocparse.mly"
            ( Float )
# 275 "nanocparse.ml"
               : 'typ_rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "nanocparse.mly"
                                ( []     )
# 281 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list_rule) in
    Obj.repr(
# 47 "nanocparse.mly"
                                ( _1::_2 )
# 289 "nanocparse.ml"
               : 'stmt_list_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 50 "nanocparse.mly"
                                                          ( Expr _1         )
# 296 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list_rule) in
    Obj.repr(
# 51 "nanocparse.mly"
                                                          ( Block _2        )
# 303 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_rule) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 52 "nanocparse.mly"
                                                          ( If (_3, _5, _7) )
# 312 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_rule) in
    Obj.repr(
# 53 "nanocparse.mly"
                                                          ( While (_3,_5)   )
# 320 "nanocparse.ml"
               : 'stmt_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 56 "nanocparse.mly"
                                  ( BoolLit _1            )
# 327 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 57 "nanocparse.mly"
                                  ( FloatLit _1           )
# 334 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 58 "nanocparse.mly"
                                  ( Literal _1            )
# 341 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "nanocparse.mly"
                                  ( Id _1                 )
# 348 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 60 "nanocparse.mly"
                                  ( Binop (_1, Add, _3)   )
# 356 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 61 "nanocparse.mly"
                                  ( Binop (_1, Sub, _3)   )
# 364 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 62 "nanocparse.mly"
                                  ( Binop (_1, Equal, _3) )
# 372 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 63 "nanocparse.mly"
                                  ( Binop (_1, Neq, _3)   )
# 380 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 64 "nanocparse.mly"
                                  ( Binop (_1, Less, _3)  )
# 388 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 65 "nanocparse.mly"
                                  ( Binop (_1, And, _3)   )
# 396 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 66 "nanocparse.mly"
                                  ( Binop (_1, Or, _3)    )
# 404 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 67 "nanocparse.mly"
                                  ( Assign (_1, _3)       )
# 412 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_rule) in
    Obj.repr(
# 68 "nanocparse.mly"
                                  ( DAssign (_1, _3)      )
# 420 "nanocparse.ml"
               : 'expr_rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_rule) in
    Obj.repr(
# 69 "nanocparse.mly"
                                  ( _2                    )
# 427 "nanocparse.ml"
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
