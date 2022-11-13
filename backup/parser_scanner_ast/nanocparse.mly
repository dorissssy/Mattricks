/* Ocamlyacc parser for NanoC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN DASSIGN LBRAC RBRAC
%token EQ NEQ LT AND OR
%token IF ELSE WHILE INT BOOL FLOAT CONST
%token RETURN COMMA
%token <int> LITERAL
%token <float> FLIT
%token <bool> BLIT
%token <string> ID
%token EOF
%token PRINTF CONSOLE
%start program_rule
%type <Ast.program> program_rule

%right ASSIGN DASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program_rule:
  vdecl_list_rule stmt_list_rule EOF { {locals=$1; body=$2} }

vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule vdecl_list_rule  { $1 :: $2 }

vdecl_rule:
  typ_rule ID SEMI { ($1, $2) }


typ_rule:
  INT       { Int  }
  | BOOL    { Bool }
  | FLOAT   { Float }

const_rule:
  CONST     { Const }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }



expr_rule:
  | BLIT                          { BoolLit $1            }
  | FLIT                          { FloatLit $1           }
  | LITERAL                       { Literal $1            }
  | ID                            { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | ID ASSIGN typ_rule expr_rule  { Assign2 ($1,$3,$4)    }
  | ID ASSIGN const_rule typ_rule expr_rule  { Assign3 ($1, $3, $4, $5)    }
  | ID DASSIGN expr_rule          { DAssign ($1, $3)      }
  | LPAREN expr_rule RPAREN       { $2                    }
  /*| PRINTF LPAREN expr_rule RPAREN { Printf $3 }*/
  | CONSOLE PRINTF expr_rule      { Printf $3 }
  | ID LBRAC expr_rule RBRAC { Array($1, $3) }
  | ID LBRAC expr_rule RBRAC LBRAC expr_rule RBRAC  { TwoDArray($1, $3, $6) }
    | ID LBRAC expr_rule RBRAC LBRAC expr_rule RBRAC LBRAC expr_rule RBRAC { ThreeDArray($1, $3, $6, $9) }
    | ID LBRAC expr_rule COMMA expr_rule RBRAC { TwoDArray($1, $3, $5) }
    | ID LBRAC expr_rule COMMA expr_rule COMMA expr_rule RBRAC { ThreeDArray($1, $3, $5, $7) }
