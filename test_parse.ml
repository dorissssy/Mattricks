open Ast
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parse.program_rule Scanner.token lexbuf in
  print_endline (string_of_program program)
