(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and f32_t      = L.double_type context
  and matrix_t   = L.array_type
  in

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> f32_t
    (* | A.IntMat(row, col) -> matrix_t (matrix_t i32_t col) row *)
    | A.IntMat1D(tp, row) ->
      (match tp with
      | A.Int -> matrix_t i32_t row
      | A.Float -> matrix_t f32_t row
      | A.Bool -> matrix_t i1_t row
      | A.IntMat1D(tp2, row2) -> matrix_t (ltype_of_typ tp2) (row2 * row)
      | _ -> raise (Failure ("Invalid type for A.IntMat1D")))
    | _ -> raise (Failure ("Invalid type for ltype_of_typ"))
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with 
        | A.Int -> L.const_int i32_t 0
        | A.Bool  -> L.const_int i1_t 0
        | A.Float -> L.const_float f32_t 0.0
        | _ -> raise (Failure ("Invalid global_var init"))
      in 
        StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup table n = try StringMap.find n table
      with Not_found -> try StringMap.find n local_vars
        with Not_found -> StringMap.find n global_vars
    in
    (* let build_2D_array = function
        A.IntMat(row, col) -> L.build_array_alloca (L.array_type i32_t col) (L.const_int i32_t row) "matrix" builder
      | _ -> raise (Failure "Invalid type for 2D array")
    in *)

    (* Construct code for an expression; return its value *)
    let rec build_expr builder (table : 'a StringMap.t) ((_, e) : sexpr) = match e with
        SLiteral i  -> (table, L.const_int i32_t i)
      | SBoolLit b  -> (table, L.const_int i1_t (if b then 1 else 0))
      | SId s       -> (table, L.build_load (lookup table s) s builder)
      | SFloatLit l -> (table, L.const_float f32_t l)
      | SAssign (s, e) -> let (new_table, e') = build_expr builder table e in
        ignore(L.build_store e' (lookup table s) builder); (new_table, e')
      | SBinop (e1, op, e2) ->
        let (_, e1') = build_expr builder table e1
        and (_, e2') = build_expr builder table e2 in
        let op_command = 
          match e1 with
          | (A.Float, _) -> (match op with
              A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Times   -> L.build_fmul
            | A.Divide  -> L.build_fdiv
            | A.Equal   -> L.build_fcmp L.Fcmp.Ueq
            | A.Neq     -> L.build_fcmp L.Fcmp.Une
            | A.Less    -> L.build_fcmp L.Fcmp.Ult
            | A.More    -> L.build_fcmp L.Fcmp.Ugt
            | A.LessEqual -> L.build_fcmp L.Fcmp.Ule
            | A.MoreEqual -> L.build_fcmp L.Fcmp.Uge
            | _ -> raise (Failure ("Invalid operator")))
          | _ -> (match op with
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Times   -> L.build_mul
            | A.Divide  -> L.build_sdiv
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.More    -> L.build_icmp L.Icmp.Sgt
            | A.LessEqual -> L.build_icmp L.Icmp.Sle
            | A.MoreEqual -> L.build_icmp L.Icmp.Sge
            | _ -> raise (Failure("Invalid operator")))
        in 
        let e' = op_command e1' e2' "tmp" builder in
        (table, e')
      | SPrintf (e) ->
        let (new_table, e') = build_expr builder table e in
        let e'' = L.build_call printf_func [| int_format_str ; e' |]
          "printf" builder in
        (new_table, e'')
      | SFPrintf (e) ->
        let (new_table, e') = build_expr builder table e in
        let e'' = L.build_call printf_func [| float_format_str ; e' |]
          "printf" builder in
        (new_table, e'')
      | STwoDArrayAccess(id, e1, e2)->
        let (new_table, e1') = build_expr builder table e1 in
        let (new_table2, e2') = build_expr builder table e2 in
        let e' = L.build_gep (lookup table id) [| e1'; e2' |] "tmp" builder in
        (new_table, e')
      | SCall(f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let actuals = List.rev (List.map (build_expr builder table) (List.rev args)) in
        let result = (match fdecl.srtyp with
          | _ -> f ^ "_result") in
        let new_table = List.fold_left (fun m (m', _) -> StringMap.union (fun _ _ _ -> None) m m') table actuals in
        let actuals = List.map snd actuals in
        (new_table, L.build_call fdef (Array.of_list actuals) result builder)
       | SOneDArrayAssign(id, idx, e1) ->
        let (new_table, e1') = build_expr builder table e1 in
        let (new_table2, idx') = build_expr builder table idx in
        let e' =  (L.build_gep (lookup table id) [| (L.const_int i32_t 0);  idx' |] "tmp" builder)  in
        ignore(L.build_store e1' e' builder); (new_table, e')
       | SArrayAccess(id, e) ->
        let (new_table, e') = build_expr builder table e in
        let e'' = L.build_load (L.build_gep (lookup table id) [| (L.const_int i32_t 0);  e' |] "tmp" builder) "tmp" builder in
        (new_table, e'')
       | SAnyArrayAccess(id, e1, e2) ->
        let (new_table, e1') = build_expr builder table e1 in
        let (new_table2, e2') = build_expr builder table e2 in
        let e' = L.build_load (L.build_gep (lookup table id) [| e1'; e2' |] "tmp" builder) "tmp" builder in
        (new_table, e')
       | STwoDArrayAssign(id, idx1, idx2, value)->
        let (new_table, value') = build_expr builder table value in
        let (new_table2, idx1') = build_expr builder table idx1 in
        let (new_table3, idx2') = build_expr builder table idx2 in
        let e' =  (L.build_gep (lookup table id) [| idx1'; idx2' |] "tmp" builder)  in
        ignore(L.build_store value' e' builder); (new_table, value')
        | _ -> raise (Failure ("Invalid expr"))

    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt  (table : 'a StringMap.t) builder (s : sstmt) = match s with
            SBlock sl -> let b, _ = List.fold_left (fun (b, m) s -> build_stmt m b s) (builder, table) sl in (b, table)
          | SExpr e -> ignore(build_expr builder table e); builder, table
          | SReturn e -> let (new_table, e') = build_expr builder table e in
              ignore (L.build_ret e' builder); (builder, new_table)
      | SIIf (predicate, then_stmt) ->
        let (_, bool_val) = build_expr builder table predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt table (L.builder_at_end context then_bb) then_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb end_bb builder);
        (L.builder_at_end context end_bb, table)
      | SIf (predicate, then_stmt, else_stmt) ->
        let (_, bool_val) = build_expr builder table predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt table (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt table (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        (L.builder_at_end context end_bb, table)

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let (_, bool_val) = build_expr while_builder table predicate in

        let body_bb = L.append_block context "while_body" the_function in
        let (build_body, _) = build_stmt table (L.builder_at_end context body_bb) body in
        add_terminal build_body build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        (L.builder_at_end context end_bb, table)

      | SBindAssign (tp, s, e) -> let (new_table, e') = build_expr builder table e in
        let added_var_list = L.build_alloca (ltype_of_typ tp) s builder in
        ignore(L.build_store e' added_var_list builder); (builder, StringMap.add s added_var_list new_table)

      | SDeclareOneDArray(v, t) ->
        match t with
        | IntMat1D(Int, _) ->
          let added_var_list = L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 0) "tmp" builder in
          (builder, StringMap.add v added_var_list table)
        | IntMat1D(Float, _) ->
          let added_var_list = L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 1766) "tmp" builder in
          (builder, StringMap.add v added_var_list table)
        | IntMat1D(Bool, _) ->
          let added_var_list = L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 3343) "tmp" builder in
          (builder, StringMap.add v added_var_list table)
        | IntMat1D(IntMat1D(_tp, dimension), _) ->
          let added_var_list = 
            match _tp with 
            | Int -> L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 5454) "tmp" builder 
            | Float -> L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 6454) "tmp" builder 
            | Bool -> L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 7454) "tmp" builder
            | _ -> raise (Failure ("SDeclareOneDArray type"))
          in
          (builder, StringMap.add v added_var_list table)
(*        let added_var_list = L.build_alloca (ltype_of_typ t) v builder in*)
(*        (builder, StringMap.add v added_var_list table)*)
        | _ -> raise (Failure ("Invalid statement"))

    in
    (* Build the code for each statement in the function *)
    let builder,_= build_stmt StringMap.empty builder (SBlock fdecl.sbody)

    in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
