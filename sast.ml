(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type smat_typ = SMtype of typ * int * int

type smat_expr =
  | SMatLiteral of int

type smat =
    None
  | SMatValue of smat_expr
  | SMat of smat list

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SAssignMat of string * smat_typ * smat
  | SAssign2 of string * typ * sexpr
  | SAssign3 of string * const_ty * typ * sexpr
  | SDAssign of string * sexpr
  | SArrayAccess of string * sexpr
  | STwoDArrayAccess of string * sexpr * sexpr
  | SThreeDArrayAccess of string * sexpr * sexpr * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SPrintf of sexpr
  | SFPrintf of sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr
  | SBindAssign of typ * string * sexpr


(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}


type sprogram = bind list * sfunc_def list


(* Pretty-printing functions *)

let string_of_smat_expr = function
  | SMatLiteral(l) -> string_of_int l

let rec string_of_smat = function
  | SMat(m) -> 
    "\n[\n" ^ String.concat ", " (List.map string_of_smat m) ^ "\n]"
  | SMatValue(e) -> string_of_smat_expr e
  | None -> ""


let string_of_smat_typ = function
  SMtype(t, x, y) -> string_of_typ t ^ "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFloatLit(l) -> string_of_float l ^ "0"
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SAssignMat(v, t, m) -> v ^ " = " ^ string_of_smat_typ t ^ string_of_smat m

      | SAssign2(v, t, e) -> v ^ " = " ^ string_of_typ t ^ " " ^ string_of_sexpr e
      | SAssign3(v, t, t2, e) -> v ^ " = " ^ string_of_const t ^ " " ^ string_of_typ t2 ^ " " ^ string_of_sexpr e
      | SDAssign(v, e) -> v ^ " := " ^ string_of_sexpr e
      | SArrayAccess(v, e) -> v ^ "[" ^ string_of_sexpr e ^ "]"
      | STwoDArrayAccess(v, e, e2) -> v ^ "[" ^ string_of_sexpr e ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "]"
      | SThreeDArrayAccess(v, e, e2, e3) -> v ^ "[" ^ string_of_sexpr e ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "]" ^ "[" ^ string_of_sexpr e3 ^ "]"
      | SPrintf(e) -> "console << (" ^ string_of_sexpr e ^ ")" ^ ";"
      | SFPrintf(e) -> "console << (" ^ string_of_sexpr e ^ ")" ^ ";"
      | SCall(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"


let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SReturn(e) -> "return " ^ string_of_sexpr e ^ ";\n"
  | SBindAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_sexpr e ^ ";\n"

let string_of_sfdecl fdecl = "function " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ") gives "^ string_of_typ fdecl.srtyp ^"\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
