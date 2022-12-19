/* Ocamlyacc parser for NanoC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE ASSIGN DASSIGN LBRAC RBRAC
%token PLUS MINUS TIMES DIVIDE MODULUS 
%token EQ NEQ LT MT LTE MTE AND OR
%token IF ELSE WHILE INT BOOL FLOAT CONST
%token RETURN COMMA FUNCTION GIVES
%token <int> LITERAL
%token <float> FLIT
%token INTMAT
%token <bool> BLIT
%token <string> ID
%token EOF
%token PRINTF CONSOLE CONSOLEF
%start program_rule
%type <Ast.program> program_rule

%right ASSIGN DASSIGN 
%right ELSE
%left OR
%left AND 
%left EQ NEQ
%left LT MT LTE MTE
%left PLUS MINUS COMMA

%%

program_rule:
  /*vdecl_list_rule stmt_list_rule EOF { {locals=$1; body=$2} } */
  decls_rule EOF { $1 }

/* (globals, func_defs) */
decls_rule:
   { ([], []) } /* nothing */ 
 | vdecl_rule SEMI decls_rule { (($1 :: fst $3), snd $3) }
 | fdecl_rule decls_rule { (fst $2, ($1 :: snd $2)) }


vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule SEMI vdecl_list_rule  { $1 :: $3 }

vdecl_rule:
  typ_rule ID  { ($1, $2) }

/* function declaration rule */
fdecl_rule:
  FUNCTION ID LPAREN formals_opt RPAREN GIVES typ_rule LBRACE vdecl_list_rule stmt_list_rule RBRACE 
  {
    {
      rtyp=$7;
      fname=$2;
      formals=$4;
      locals=$9;
      body=$10
    }
  }

/* formals_opt */
formals_opt:
    { [] } /*nothing*/ 
  | formals_list { $1 }

formals_list:
    vdecl_rule { [$1] }
  | vdecl_rule COMMA formals_list { $1::$3 }

typ_rule:
    INT     { Int  }
  | BOOL    { Bool }
  | FLOAT   { Float }
  | one_d_array_rule { $1 }

one_d_array_rule:
  | INTMAT typ_rule LBRAC LITERAL RBRAC { IntMat1D ($2,$4) }

stmt_list_rule:
    /* nothing */             { []     }
  | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule                  { IIf ($3, $5)    }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }
  | RETURN expr_rule SEMI                        { Return $2      }
  | ID ASSIGN typ_rule expr_rule SEMI          { BindAssign ($3, $1, $4) }
  | ID ASSIGN INTMAT LPAREN LITERAL COMMA LITERAL RPAREN SEMI { DeclareMat($1, $5, $7) }
  /*| ID LBRAC LITERAL RBRAC LBRAC LITERAL RBRAC ASSIGN expr_rule SEMI { TwoDArrayAssign ($1, $3, $6, $9) }*/
  | ID ASSIGN one_d_array_rule SEMI { DeclareOneDArray ($1, $3) }

const_rule:
  CONST     { Const }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | FLIT                          { FloatLit $1           }
  | LITERAL                       { Literal $1            }
  | ID                            { Id $1                 }
  | expr_rule TIMES expr_rule     { Binop ($1, Times, $3 )}
  | expr_rule DIVIDE expr_rule    { Binop ($1, Divide, $3 )}
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule MODULUS expr_rule   { Binop ($1, Modulus, $3) }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule MT expr_rule        { Binop ($1, More, $3)  }
  | expr_rule LTE expr_rule       { Binop ($1, LessEqual, $3)  }
  | expr_rule MTE expr_rule       { Binop ($1, MoreEqual, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | ID ASSIGN const_rule typ_rule expr_rule  { Assign3 ($1, $3, $4, $5)    }
  | LPAREN expr_rule RPAREN       { $2                    }
  | ID LBRAC expr_rule RBRAC ASSIGN expr_rule { OneDArrayAssign ($1, $3, $6) }
  | ID LBRAC expr_rule RBRAC { ArrayAccess($1, $3) }
  | ID LBRAC expr_rule RBRAC LBRAC expr_rule RBRAC { AnyArrayAccess($1, $3, $6) }
  | CONSOLE PRINTF expr_rule       { Printf $3 }
  | CONSOLEF PRINTF expr_rule       { FPrintf $3 }
  | CONSOLE PRINTF expr_rule      { Printf $3             }
  | CONSOLEF PRINTF expr_rule     { FPrintf $3            }
  | ID LPAREN args_opt RPAREN     { Call ($1, $3)         }

  /* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr_rule  { [$1] }
  | expr_rule COMMA args { $1::$3 }
