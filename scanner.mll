(* Ocamllex scanner for NanoC *)

{ open Nanocparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let float_re = ['+' '-']?['0'- '9']+ ['.'] ['0' - '9']+
let float_e_re = ['+'  '-']? ['.']['0' - '9']+
let format_re = ['+'  '-' '.']?[ '0' - '9']+ ['.']
let s1 = ['+'  '-']?['0' - '9']+ ['e' 'E'] ['+'  '-']? ['0' - '9']+
let s2 = ['+'  '-']?'0' ['.']['0' - '9']+ ['e' 'E'] ['+'  '-']? ['0' - '9']+
let s3 = float_re ['e' 'E'] ['+'  '-']? ['0' - '9']+
let s4 = float_e_re ['e' 'E'] ['+'  '-']? ['0' - '9']+
let s5 = format_re ['e' 'E'] ['+'  '-']? ['0' - '9']+
let s6 = ['+'  '-']?['0' - '9']+ ['.']
let float_format = float_re | float_e_re | s1 | s2 | s3 | s4 | s5 | s6

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"        { comment lexbuf }           (* Comments *)
| '('         { LPAREN }
| ')'         { RPAREN }
| '{'         { LBRACE }
| '}'         { RBRACE }
| ';'         { SEMI }
| ','         { COMMA }
| '+'         { PLUS }
| '-'         { MINUS }
| '='         { ASSIGN }
| ":="        { DASSIGN }
| "=="        { EQ }
| "!="        { NEQ }
| '<'         { LT }
| "&&"        { AND }
| "||"        { OR }
| "if"        { IF }
| "else"      { ELSE }
| "while"     { WHILE }
| "return"    { RETURN }
| '['         { LBRAC }
| ']'         { RBRAC }
| "int"       { INT }
| "bool"      { BOOL }
| "float"     { FLOAT }
| "True"      { BLIT(true)  }
| "true"      { BLIT(true)  }
| "False"     { BLIT(false) }
| "false"     { BLIT(false) }
| "<<"        { PRINTF }
| "console"   { CONSOLE }
| "const"     { CONST }
| "function"  { FUNCTION }
| "gives"     { GIVES }
| float_format as lem { FLIT(float_of_string lem) }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
