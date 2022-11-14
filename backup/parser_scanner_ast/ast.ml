type bop = Add | Sub | Equal | Neq | Less | And | Or

type typ = 
    Int
  | Bool
  | Float

type mat_typ = Mtype of typ * int * int

(* name const_ty ty? *)
type const_ty = Const

type expr =
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | Id of string
  | Binop of expr * bop * expr
  | AssignMat of string * mat_typ
  | Assign of string * expr
  | Assign2 of string * typ * expr
  (* const assignment = Assign3 *)
  | Assign3 of string * const_ty * typ * expr
  | DAssign of string * expr
  | Printf of expr
  | Array of string * expr
  | TwoDArray of string * expr * expr
  | ThreeDArray of string * expr * expr * expr
  | Return of expr


type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type bind = typ * string
type declaration = typ * string * expr

(* type program = {
  locals: bind list;

  body: stmt list;
} *)

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list; (* function argument list *)
  locals: bind list;
  body: stmt list;
}
type program = bind list * func_def list




(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"

let string_of_mat_typ = function
  Mtype(t, x, y) -> string_of_typ t ^ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let string_of_const = function
    Const -> "const"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | AssignMat(v, e) -> v ^ " = " ^ string_of_mat_typ e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Assign2(v, t, e) -> v ^ " = " ^ string_of_typ t ^ " " ^ string_of_expr e
  | Assign3(v, c, t, e) -> v ^ " = " ^ string_of_const c ^ " " ^ string_of_typ t ^ " " ^ string_of_expr e
  | DAssign(v, e) -> v ^ " := " ^ string_of_expr e
  | Printf(e) -> "console << " ^ string_of_expr e
  | Array(s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | TwoDArray(s, e1, e2) -> s ^ "[" ^ string_of_expr e1 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]"
  | ThreeDArray(s, e1, e2, e3) -> s ^ "[" ^ string_of_expr e1 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]" ^ "[" ^ string_of_expr e3 ^ "]"
  | Return(ret) -> "return " ^ string_of_expr ret

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s



let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"
(* let string_of_adecl (t, id, e) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e ^ ";\n"
let string_of_dadecl (id, e) = id ^ " := " ^ string_of_expr e ^ ";\n" *)

(* let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^

  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "\n" *)
(* 

let string_of_program_output fdecl =
  String.concat "" (List.map string_of_stmt fdecl.body) *)

  let string_of_fdecl fdecl =
    string_of_typ fdecl.rtyp ^ " " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
    ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.locals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"
let string_of_program (vars, funcs) =
    "\n\nParsed program: \n\n" ^
    String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs)

(* Below is used for the output*)


