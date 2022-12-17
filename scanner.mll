(* Ocamllex scanner for NanoC *)

{ open Parse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let num = ['0'-'9']
let e = ['e' 'E']
let sign = ['-' '+']
let dot = ['.']
let decimal = num+ dot num* | num* dot num+
let int_or_decimal = num+ | decimal
let with_e = int_or_decimal e sign num+ | int_or_decimal e num+
let return_value = decimal | with_e

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
| '*'         { TIMES }
| '/'         { DIVIDE }
| '%'         { MODULUS }
| '='         { ASSIGN }
| ":="        { DASSIGN }
| "=="        { EQ }
| "!="        { NEQ }
| '<'         { LT } (* Less than *)
| '>'         { MT } (* More than *)
| "<="        { LTE } (* Less than or equal to *)
| ">="        { MTE } (* More than or equal to *)
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
| "consolef"  { CONSOLEF }
| "const"     { CONST }
| "function"  { FUNCTION }
| "gives"     { GIVES }
| '-'?return_value as lem { FLIT(float_of_string lem) }
| '-'?digit+ as lem  { LITERAL(int_of_string lem) }
| ('_' | letter) (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
