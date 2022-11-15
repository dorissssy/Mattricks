

type bop = Add | Sub | Equal | Neq | Less | And | Or

type typ = Int | Bool | Float

type expr =
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | DAssign of string * expr


type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type bind = typ * string
type declaration = typ * string * expr

type program = {
  locals: bind list;

  body: stmt list;
}


(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | DAssign(v, e) -> v ^ " := " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"
let string_of_adecl (t, id, e) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ ";\n"
let string_of_dadecl (id, e) = id ^ " := " ^ string_of_expr e ^ ";\n"

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^

  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "\n"


