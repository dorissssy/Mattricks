(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let int_fd =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [(Int, "x")];
      locals = []; body = [] } StringMap.empty
  in
let bool_fd =
    StringMap.add "print" {
      rtyp = Bool;
      fname = "print";
      formals = [(Bool, "x")];
      locals = []; body = [] } int_fd
  in
  let built_in_decls =
      StringMap.add "print" {
        rtyp = Float;
        fname = "print";
        formals = [(Float, "x")];
        locals = []; body = [] } bool_fd
    in
  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s map =
      try StringMap.find s map
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
    let check_one_dim_array_access arr_id d map =
        let arr_type = type_of_identifier arr_id map in
       match d with
       | SLiteral idx ->(match arr_type with
            Vtype(tp, dim) -> if dim > idx or idx < 0 then tp else raise (Failure ("Array access to " ^ arr_id ^ "[" ^ string_of_int idx ^ "] out of bound"))
            | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ string_of_int idx ^ "] is not an array")))
       | SId id -> (let id_type = type_of_identifier id map
       in
        match id_type with
        | Int -> (match arr_type with
            Vtype(tp, dim) -> tp
           | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ id ^ "] is not an array")))
        | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ id ^ "] is not an integer")))
       | _ -> raise (Failure ("Array subscript must be an integer"))
    in
    let check_two_dim_array_access arr_id d1 d2 map =
        let arr_type = type_of_identifier arr_id map in
       match d1 with
       | SLiteral idx1 -> (match d2 with
         | SLiteral idx2 -> (match arr_type with
                Mtype(tp, dim1, dim2) -> if dim1 > idx1 or idx1 < 0 then tp else raise (Failure ("Array access to " ^ arr_id ^ "[" ^ string_of_int idx1 ^ "][" ^ string_of_int idx2 ^ "] out of bound"))
                | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ string_of_int idx1 ^ "][" ^ string_of_int idx2 ^ "] is not an array")))
         | SId id -> (let id_type = type_of_identifier id map
         in
          match id_type with
          | Int -> (match arr_type with
              Mtype(tp, dim1, dim2) -> tp
             | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ string_of_int idx1 ^ "][" ^ id ^ "] is not an array")))
          | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ string_of_int idx1 ^ "][" ^ id ^ "] is not an integer"))))
      | SId idx1 -> (let id_type = type_of_identifier idx1 map
            in
            match id_type with
            | Int -> (match d2 with
                SLiteral idx2 -> (match arr_type with
                    Mtype(tp, dim1, dim2) -> tp
                   | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ idx1 ^ "][" ^ string_of_int idx2 ^ "] is not an array")))
                | SId idx2 -> (let id_type = type_of_identifier idx2 map
                in
                match id_type with
                | Int -> (match arr_type with
                    Mtype(tp, dim1, dim2) -> tp
                   | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ idx1 ^ "][" ^ idx2 ^ "] is not an array")))
                | _ -> raise (Failure ("Array access to " ^ arr_id ^ "[" ^ idx1 ^ "][" ^ idx2 ^ "] is not an integer")))
                | _ -> raise (Failure ("Array subscript must be an integer")))
            | _ -> raise (Failure ("Array subscript must be an integer")))
    in
    let check_three_dim_array_access v dim1 dim2 dim3 map =
        let arr_type = type_of_identifier v  map in
       match dim1 with
       | SLiteral idx1 -> (match dim2 with
         | SLiteral idx2 -> (match dim3 with
            | SLiteral idx3 -> (match arr_type with
                Ttype(tp, dim1, dim2, dim3) -> if dim1 > idx1 or idx1 < 0 then tp else raise (Failure ("Array access to " ^ v ^ "[" ^ string_of_int idx1 ^ "][" ^ string_of_int idx2 ^ "][" ^ string_of_int idx3 ^ "] out of bound"))
                | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ string_of_int idx1 ^ "][" ^ string_of_int idx2 ^ "][" ^ string_of_int idx3 ^ "] is not an array")))
            | SId id -> (let id_type = type_of_identifier id map
            in
            match id_type with
            | Int -> (match arr_type with
                Ttype(tp, dim1, dim2, dim3) -> tp
               | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ string_of_int idx1 ^ "][" ^ string_of_int idx2 ^ "][" ^ id ^ "] is not an array")))
            | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ string_of_int idx1 ^ "][" ^ string_of_int idx2 ^ "][" ^ id ^ "] is not an integer"))))
         | SId idx2 -> (let id_type = type_of_identifier idx2 map
            in
            match id_type with
            | Int -> (match dim3 with
                SLiteral idx3 -> (match arr_type with
                    Ttype(tp, dim1, dim2, dim3) -> tp
                   | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ string_of_int idx1 ^ "][" ^ idx2 ^ "][" ^ string_of_int idx3 ^ "] is not an array")))
                | SId idx3 -> (let id_type = type_of_identifier idx3 map
                in
                match id_type with
                | Int -> (match arr_type with
                    Ttype(tp, dim1, dim2, dim3) -> tp
                   | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ string_of_int idx1 ^ "][" ^ idx2 ^ "][" ^ idx3 ^ "] is not an array")))
                | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ string_of_int idx1 ^ "][" ^ idx2 ^ "][" ^ idx3 ^ "] is not an integer")))
                | _ -> raise (Failure ("Array subscript must be an integer")))
            | _ -> raise (Failure ("Array subscript must be an integer"))))
        | SId idx1 -> (let id_type = type_of_identifier idx1 map
            in
            match id_type with
            | Int -> (match dim2 with
                SLiteral idx2 -> (match dim3 with
                    SLiteral idx3 -> (match arr_type with
                        Ttype(tp, dim1, dim2, dim3) -> tp
                       | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ idx1 ^ "][" ^ string_of_int idx2 ^ "][" ^ string_of_int idx3 ^ "] is not an array")))
                    | SId idx3 -> (let id_type = type_of_identifier idx3 map
                    in
                    match id_type with
                    | Int -> (match arr_type with
                        Ttype(tp, dim1, dim2, dim3) -> tp
                       | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ idx1 ^ "][" ^ string_of_int idx2 ^ "][" ^ idx3 ^ "] is not an array")))
                    | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ idx1 ^ "][" ^ string_of_int idx2 ^ "][" ^ idx3 ^ "] is not an integer")))
                    | _ -> raise (Failure ("Array subscript must be an integer")))
                | SId idx2 -> (let id_type = type_of_identifier idx2 map
                in
                match id_type with
                | Int -> (match dim3 with
                    SLiteral idx3 -> (match arr_type with
                        Ttype(tp, dim1, dim2, dim3) -> tp
                       | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ idx1 ^ "][" ^ idx2 ^ "][" ^ string_of_int idx3 ^ "] is not an array")))
                    | SId idx3 -> (let id_type = type_of_identifier idx3 map
                    in
                    match id_type with
                    | Int -> (match arr_type with
                        Ttype(tp, dim1, dim2, dim3) -> tp
                       | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ idx1 ^ "][" ^ idx2 ^ "][" ^ idx3 ^ "] is not an array")))
                    | _ -> raise (Failure ("Array access to " ^ v ^ "[" ^ idx1 ^ "][" ^ idx2 ^ "][" ^ idx3 ^ "] is not an integer")))
                    | _ -> raise (Failure ("Array subscript must be an integer")))
                | _ -> raise (Failure ("Array subscript must be an integer"))))
            | _ -> raise (Failure ("Array subscript must be an integer")))
    in
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr map expr = match expr with
        Literal l -> (Int, SLiteral l, map)
      | BoolLit l -> (Bool, SBoolLit l, map)
      | FloatLit l -> (Float, SFloatLit l, map)
      | Id var -> (type_of_identifier var map, SId var, map)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var map
        and (rt, e', map') = check_expr map e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')), map')
      | Binop(e1, op, e2) as e ->
        let (t1, e1', map1) = check_expr map e1
        in
        let (t2, e2', map2) = check_expr map1 e2
        in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')), map2)
        else raise (Failure err)
      | ArrayAccess(id, dim) as ex ->
                let lt = type_of_identifier id map
                and (rt, e', map1) = check_expr map dim in
                let err = "illegal array access " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
                in
                (check_one_dim_array_access id e' map1, SArrayAccess(id, (rt, e')), map1)
      | TwoDArrayAccess(v, fst, scd) as ex ->
                let lt = type_of_identifier v map
                and (rt1, e1', map1) = check_expr map fst
                in
                let (rt2, e2', map2) = check_expr map1 scd in
                let err = "illegal array access " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt1 ^ " in " ^ string_of_expr ex
                in
                (check_two_dim_array_access v e1' e2' map2, STwoDArrayAccess(v, (rt1, e1'), (rt2, e2')), map2)
     | ThreeDArrayAccess(v, fst, scd, thd) as ex ->
                let lt = type_of_identifier v map
                and (rt1, e1', map1) = check_expr map fst
                in let (rt2, e2', map2) = check_expr map1 scd
                in let (rt3, e3', map3) = check_expr map2 thd in
                let err = "illegal array access " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt1 ^ " in " ^ string_of_expr ex
                in
                (check_three_dim_array_access v e1' e2' e3' map3, SThreeDArrayAccess(v, (rt1, e1'), (rt2, e2'), (rt3, e3')), map3)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e', map) = check_expr map e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'), map)
    in

    let check_bool_expr map e =
    			let (t, e', map') = check_expr map e in
    			match t with
    				Bool -> (t, e')
    			| _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list map = function
    [] -> ([], map)
    | Block sl :: sl' -> check_stmt_list map (sl @ sl')
    | s :: sl -> let (s1, map1) = check_stmt map s in
    						 let (s2, map2) = check_stmt_list map1 sl in
    						 (s1 :: s2, map2)
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt map = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
         Block sl -> (SBlock(fst (check_stmt_list map sl)), map)
       | Expr e -> let (typ, sexpr, new_map) = check_expr map e in
       						(SExpr (typ, sexpr), new_map)
      | If(e, st1, st2) ->
      		let sthen, map1 = check_stmt map st1 in
      		let selse, map2 = check_stmt map1 st2 in
      		(SIf(check_bool_expr map2 e, sthen, selse), map2)
      | While(e, stList) -> SWhile(check_bool_expr map e, fst (check_stmt map stList)), map
      | Printf(e) ->
        let (typ, sexpr, new_map) = check_expr map e in
        (SPrintf(typ, sexpr), new_map)
      | Return e ->
        let (t, e', map1) = check_expr map e in
        if t = func.rtyp then (SReturn (t, e'), map1)
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
     | BindAssign(tp, id, e) ->
        let (t, e', map1) = check_expr map e in
        let err = "illegal assignment " ^ string_of_typ t ^ " = " ^
                  string_of_typ tp ^ " in " ^ string_of_expr e
        in
        if StringMap.mem id map1 then
          let err = "Duplicated declaration " ^ string_of_typ t ^ " = " ^
                                      string_of_typ tp ^ " in " ^ string_of_expr e
                     in
                    raise (Failure err)
        else
            (SBindAssign(tp, id, (t, e')), StringMap.add id tp map)
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
        sbody = fst (check_stmt_list symbols func.body)
    }
  in
  (globals, List.map check_func functions)
