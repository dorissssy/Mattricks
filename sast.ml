(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SAssignMat of string * typ
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
  | SOneDArrayAssign of string * sexpr * sexpr
  | SAnyArrayAccess of string * sexpr * sexpr
  | STwoDArrayAssign of string * sexpr * sexpr * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIIf of sexpr * sstmt
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr
  | SBindAssign of typ * string * sexpr
  (* | SDeclareMat of string * int * int *)
  (* | STwoDArrayAssign of string * int * int * sexpr *)
  | SDeclareOneDArray of string * typ



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
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ 
  (match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFloatLit(l) -> string_of_float l ^ "0"
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SAssignMat(v, t) -> v ^ " = " ^ string_of_typ t
      | SAssign2(v, t, e) -> v ^ " = " ^ string_of_typ t ^ " " ^ string_of_sexpr e
      | SAssign3(v, t, t2, e) -> v ^ " = " ^ string_of_const t ^ " " ^ string_of_typ t2 ^ " " ^ string_of_sexpr e
      | SDAssign(v, e) -> v ^ " := " ^ string_of_sexpr e
      | SPrintf(e) -> "console << (" ^ string_of_sexpr e ^ ")" ^ ";"
      | SFPrintf(e) -> "console << (" ^ string_of_sexpr e ^ ")" ^ ";"
      | SCall(f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"

      | SArrayAccess(v, e) -> v ^ "[" ^ string_of_sexpr e ^ "]"
      (* | STwoDArrayAccess(v, e, e2) -> v ^ "[" ^ string_of_sexpr e ^ " , " ^ string_of_sexpr e2 ^ "]" *)
      (* | SThreeDArrayAccess(v, e, e2, e3) -> v ^ "[" ^ string_of_sexpr e ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "]" ^ "[" ^ string_of_sexpr e3 ^ "]" *)
      | SOneDArrayAssign(v, e, e2) -> v ^ "[" ^ string_of_sexpr e ^ "]" ^ " = " ^ string_of_sexpr e2
      | STwoDArrayAssign(v, e, e2, e3) -> v ^ "[" ^ string_of_sexpr e ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "]" ^ " = " ^ string_of_sexpr e3
      | SAnyArrayAccess(v, e, e2) -> v ^ "[" ^ string_of_sexpr e ^ "]" ^ "[" ^ string_of_sexpr e2 ^ "]"
    ) ^ ")"



let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SIIf(e, s1) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SReturn(e) -> "return " ^ string_of_sexpr e ^ ";\n"
  | SBindAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_sexpr e ^ ";\n"
  (* | SDeclareMat(v, i, j) ->  v ^ " = Intmat" ^ "[" ^ string_of_int i ^ "]" ^ "[" ^ string_of_int j ^ "]" ^ ";\n" *)
  (* | STwoDArrayAssign(v, r, c, e) -> v ^ "[" ^ string_of_int r ^ "]" ^ "[" ^ string_of_int c ^ "]" ^ " = " ^ string_of_sexpr e ^ ";\n" *)
  | SDeclareOneDArray(v, t) -> v ^ " => " ^ string_of_typ t ^ ";\n"

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
