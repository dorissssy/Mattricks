Open Ast

let tb = Hashtbl.create 123456;;


let rec eval = function
      Lit(x)            -> x
    | Binop(e1, op, e2) ->
        let v1  = eval e1 in
        let v2 = eval e2 in
        (match op with
        Add -> v1 + v2
        | Sub -> v1 - v2
        | Mul -> v1 * v2
        | Div -> v1 / v2
        | Semicolon -> v2)
    | Id(x) -> 
        if Hashtbl.mem tb x then 
            Hashtbl.find tb x 
        else
            raise (Failure "Variable not found")
    | Assign(x, e) -> 
        let v = eval e in Hashtbl.replace tb x v; v
    | IIF(e1, e2) -> if eval e1 <> 0 then eval e2
    | If(e1, e2, e3) -> if eval e1 <> 0 then eval e2 else eval e3
    | While(e1, e2) -> let rec loop () = if eval e1 <> 0 then (eval e2; loop ()) in loop (); 0
    | Block(e1, e2) -> eval e1; eval e2
    | Print(e) -> let v = eval e in print_int v; print_newline (); v
    | PrintBool(b) -> print_string (if b then "true" else "false"); print_newline (); 0
    | PrintBoolNoNewline(b) -> print_string (if b then "true" else "false"); 0
    | PrintFloat(f) -> print_float f; print_newline (); 0
    | PrintFloatNoNewline(f) -> print_float f; 0
    | PrintNewline() -> print_newline (); 0

  | Assign2(x, ty, e) -> Hashtbl.replace tb x ty; ty