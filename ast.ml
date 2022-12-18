type bop = Add | Sub | Times | Divide | Equal | Neq | Less | More | LessEqual | MoreEqual | And | Or | Modulus

type typ = 
    Int
  | Bool
  | Float
  | Mtype of typ * int * int
  | Vtype of typ * int
  | Ttype of typ * int * int * int
  | IntMat of int * int
type mat_typ = Mtype of typ * int * int



type mat_expr =
  | MatLiteral of int

type mat =
    None
  | MatValue of mat_expr
  | Mat of mat list

(* name const_ty ty? *)
type const_ty = Const

type expr =
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | Id of string
  | Binop of expr * bop * expr
  | AssignMat of string * mat_typ * mat
  | Assign of string * expr
  | Assign2 of string * typ * expr
  (* const assignment = Assign3 *)
  | Assign3 of string * const_ty * typ * expr
  | DAssign of string * expr
  | ArrayAccess of string * expr
  | TwoDArrayAccess of string * expr * expr
  | ThreeDArrayAccess of string * expr * expr * expr
  | Call of string * expr list
  | Printf of expr
  | FPrintf of expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Return of expr
  | BindAssign of typ * string * expr
  | DeclareMat of string * int * int
  | TwoDArrayAssign of string * int * int * expr

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
  | Times -> "*"
  | Divide -> "/"
  | Modulus -> "mod"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | More -> ">"
  | LessEqual -> "<="
  | MoreEqual -> ">="
  | And -> "&&"
  | Or -> "||"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Mtype(t, x, y) -> string_of_typ t ^ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
  | Vtype(t, x) -> string_of_typ t ^ "(" ^ string_of_int x ^ ")"
  | Ttype(t, x, y, z) -> string_of_typ t ^ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ string_of_int z ^ ")"
  | IntMat(x, y) -> "int" ^ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let string_of_mat_typ = function
  Mtype(t, x, y) -> string_of_typ t ^ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let string_of_const = function
    Const -> "const"

let string_of_mat_expr = function
  | MatLiteral(l) -> string_of_int l

let rec string_of_mat = function
  |  Mat(m) -> 
    "\n[\n" ^ String.concat ", " (List.map string_of_mat m) ^ "\n]"
  |  MatValue(e) -> string_of_mat_expr e
  | None -> ""

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FloatLit(l) -> string_of_float l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | AssignMat(v, e, m) -> v ^ " = " ^ string_of_mat_typ e ^ string_of_mat m
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Assign2(v, t, e) -> v ^ " = " ^ string_of_typ t ^ " " ^ string_of_expr e
  | Assign3(v, c, t, e) -> v ^ " = " ^ string_of_const c ^ " " ^ string_of_typ t ^ " " ^ string_of_expr e
  | DAssign(v, e) -> v ^ " := " ^ string_of_expr e
  | ArrayAccess(s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | TwoDArrayAccess(s, x, y) -> s ^ "[" ^ string_of_expr x ^ "," ^ string_of_expr y ^ "]"
  | ThreeDArrayAccess(s, e1, e2, e3) -> s ^ "[" ^ string_of_expr e1 ^ "]" ^ "[" ^ string_of_expr e2 ^ "]" ^ "[" ^ string_of_expr e3 ^ "]"
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Printf(e) -> "printf(" ^ string_of_expr e ^ ")"
  | FPrintf(e) -> "fprintf(" ^ string_of_expr e ^ ")"
let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Return(ret) -> "return " ^ string_of_expr ret ^ ";\n"
  | BindAssign(v, t, e) ->   t ^ " = " ^ string_of_typ v ^ " " ^ string_of_expr e ^ ";\n"
  | DeclareMat(v, x, y) ->   v ^ " = int[" ^ string_of_int x ^ "][" ^ string_of_int y ^ "];\n"
  | TwoDArrayAssign(s, e1, e2, e3) -> s ^ "[" ^ string_of_int e1 ^ "]" ^ "[" ^ string_of_int e2 ^ "] = " ^ string_of_expr e3


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


