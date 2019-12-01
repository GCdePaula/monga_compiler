open Base
open TypedAst
open AstTypes
open Llvm

exception SurprisedPikachu of string

module NameEnv = Stdlib.Map.Make(String)

let gen_code typed_tree =

  let llctx = global_context () in
  let llmodule = create_module llctx "program" in

  (* "Runtime" constants *)
  let printf =
    let i8_t = i8_type llctx in
    let i32_t = i32_type llctx in
    let print_type = var_arg_function_type i32_t [| pointer_type i8_t |] in
    declare_function "printf" print_type llmodule
  in

  (* let int_format_str = define_global "" (const_stringz llctx "%ld\n") llmodule in *)
  (* let float_format_str = define_global "" (const_stringz llctx "%lf\n") llmodule in *)
  (* *)

  (* Helper functions *)
  let lltype_from_mongatype m_type = (* TODO complete this function. Ask what type is array*)
    match m_type with
    | Int -> i64_type llctx
    | Float -> double_type llctx
    | Bool -> i1_type llctx
    | _ -> raise (SurprisedPikachu "lltype_from_mongatype case not implemented")
  in

  let lltype_from_functype f_type =
    let params = Array.of_list
        (List.map ~f:(fun var -> lltype_from_mongatype var.t) f_type.parameters)
    in
    (match f_type.ret_type with
     | None ->
       let void_type = void_type llctx in
       function_type void_type params
     | Some t ->
       let ret_type = lltype_from_mongatype t in
       function_type ret_type params
    )
  in
  (* *)

  (* Actual code generation *)
  let rec gen_stat curr_func curr_basic_block env stat_node =

    (* Generates the value of fiven expression, returning llvalue and llbasicblock *)
    let rec gen_exp curr_bb exp_node =
      let build_bin_exp (exp1: t_exp_node) exp2 build_iop build_fop =
        let (llexp1, new_bb1) = gen_exp curr_bb exp1 in
        let (llexp2, new_bb2) = gen_exp new_bb1 exp2 in

        let builder = builder_at_end llctx new_bb2 in
        let result =
          match exp1.t, exp2.t with
          | Float, Float ->
            build_fop llexp1 llexp2 "" builder
          | _, _ ->
            build_iop llexp1 llexp2 "" builder
        in
        (result, new_bb2)
      in

      match exp_node.exp with
      | IntExp inum ->
        let llval = const_int (lltype_from_mongatype Int) inum in
        (llval, curr_bb)

      | FloatExp fnum ->
        let llval = const_float (lltype_from_mongatype Float) fnum in
        (llval, curr_bb)

      | TrueExp ->
        let llval = const_int (lltype_from_mongatype Bool) 1 in
        (llval, curr_bb)

      | FalseExp ->
        let llval = const_int (lltype_from_mongatype Bool) 0 in
        (llval, curr_bb)

      | VarExp name ->
        let llvar = NameEnv.find name env in
        let builder = builder_at_end llctx curr_bb in
        let llvarexp = build_load llvar "" builder in
        (llvarexp, curr_bb)

      | CallExp (name, args) ->
        let (llargs, new_bb) =
          let rec build_args bb exp_list =
            match exp_list with
            | [] -> ([], bb)
            | exp :: exps ->
              let (llexp, new_bb) = gen_exp bb exp in
              let (llexps, last_bb) = (build_args new_bb exps) in
              (llexp :: llexps, last_bb)
          in
          build_args curr_bb args
        in
        let builder = builder_at_end llctx new_bb in
        let llf = NameEnv.find name env in
        let res = build_call llf (Array.of_list llargs) "" builder in
        (res, new_bb)

      | AndExp _ | OrExp _ ->
        (* Creates control bb, using gen_cond, for short circuit *)
        let true_bb = append_block llctx "" curr_func in
        let false_bb = append_block llctx "" curr_func in
        gen_cond curr_bb true_bb false_bb exp_node;

        (* Adds branch instruction from then/else last block to continue block *)
        let continue_bb = append_block llctx "" curr_func in
        let _ = build_br continue_bb (builder_at_end llctx true_bb) in
        let _ = build_br continue_bb (builder_at_end llctx false_bb) in

        (* The value of the expression is given by where it came from *)
        let lltrue = const_int (lltype_from_mongatype Bool) 1 in
        let llfalse = const_int (lltype_from_mongatype Bool) 0 in
        let builder = builder_at_end llctx continue_bb in
        let incoming = [(lltrue, true_bb); (llfalse, false_bb)] in
        let result = build_phi incoming "" builder in
        (result, continue_bb)

      | UnaryNotExp exp ->
        let (llval, new_bb) = gen_exp curr_bb exp in
        let builder = builder_at_end llctx new_bb in
        let result = build_not llval "" builder in
        (result, new_bb)

      | UnaryMinusExp exp ->
        let (llval, new_bb) = gen_exp curr_bb exp in
        let builder = builder_at_end llctx new_bb in
        let result = build_neg llval "" builder in
        (result, new_bb)

      | EqExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 (build_icmp Eq) (build_fcmp Ueq)
      | NeExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 (build_icmp Ne) (build_fcmp Une)
      | GeExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 (build_icmp Sge) (build_fcmp Uge)
      | LeExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 (build_icmp Sle) (build_fcmp Ule)
      | GtExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 (build_icmp Sgt) (build_fcmp Ugt)
      | LtExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 (build_icmp Slt) (build_fcmp Ult)

      | AddExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 build_add build_fadd
      | SubExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 build_sub build_fsub
      | MulExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 build_mul build_fmul
      | DivExp (exp1, exp2) ->
        build_bin_exp exp1 exp2 build_sdiv build_fdiv


      | _ -> raise (SurprisedPikachu "Exp case not implemented yet")

    and gen_cond curr_bb true_bb false_bb cond_node =
      let build_cmp exp1 exp2 icmp fcmp =
        let (llexp1, new_bb1) = gen_exp curr_bb exp1 in
        let (llexp2, new_bb2) = gen_exp new_bb1 exp2 in

        let builder = builder_at_end llctx new_bb2 in
        let llcond =
          match exp1.t, exp2.t with
          | Float, Float ->
            build_fcmp fcmp llexp1 llexp2 "" builder
          | _, _ ->
            build_icmp icmp llexp1 llexp2 "" builder
        in

        let _ = build_cond_br llcond true_bb false_bb builder in
        ()
      in

      match cond_node.exp with
      | AndExp (exp1, exp2) ->
        let middle_bb = insert_block llctx "" true_bb in
        let _ = gen_cond curr_bb middle_bb false_bb exp1 in
        let _ = gen_cond middle_bb true_bb false_bb exp2 in
        ()

      | OrExp (exp1, exp2) ->
        let middle_bb = insert_block llctx "" true_bb in
        let _ = gen_cond curr_bb true_bb middle_bb exp1 in
        let _ = gen_cond middle_bb true_bb false_bb exp2 in
        ()

      | UnaryNotExp exp -> gen_cond curr_bb false_bb true_bb exp
      | EqExp (exp1, exp2) -> build_cmp exp1 exp2 Eq Ueq
      | NeExp (exp1, exp2) -> build_cmp exp1 exp2 Ne Une
      | GeExp (exp1, exp2) -> build_cmp exp1 exp2 Sge Uge
      | LeExp (exp1, exp2) -> build_cmp exp1 exp2 Sle Ule
      | GtExp (exp1, exp2) -> build_cmp exp1 exp2 Sgt Ugt
      | LtExp (exp1, exp2) -> build_cmp exp1 exp2 Slt Ult
      | _ ->
        let (llval, new_bb) = gen_exp curr_bb cond_node in

        let builder = builder_at_end llctx new_bb in
        let _ = build_cond_br llval true_bb false_bb builder in
        ()
    in

    (* Gets the address of given variable *)
    let gen_addr bb exp_node =
      (* let llbuilder = builder_at_end llctx bb in *)

      match exp_node.exp with
      | VarExp name ->
        NameEnv.find name env
      | _ -> raise (SurprisedPikachu "gen_addr case not implemented yet")
    in


    match stat_node with
    | IfElseStat (cond, then_block, Some else_block) ->
      (* Creates basic blocks for then, else, and the following block *)
      let true_bb = append_block llctx "" curr_func in
      let false_bb = append_block llctx "" curr_func in
      let continue_bb = append_block llctx "" curr_func in

      (* Generates code for then block and else block*)
      let (then_end, _) = gen_block curr_func true_bb env then_block in
      let (else_end, _) = gen_block curr_func false_bb env else_block in

      (* Adds branch instruction from then/else last block to continue block *)
      let _ = build_br continue_bb (builder_at_end llctx then_end) in
      let _ = build_br continue_bb (builder_at_end llctx else_end) in

      (* In current basic block, generate code for conditional jump *)
      gen_cond curr_basic_block true_bb false_bb cond;
      continue_bb

    | IfElseStat (cond, then_block, None) ->
      (* Creates basic blocks for then, else, and the following block *)
      let true_bb = append_block llctx "" curr_func in
      let continue_bb = append_block llctx "" curr_func in

      (* Generates code for then block and else block*)
      let (then_end, _) = gen_block curr_func true_bb env then_block in

      (* Adds branch instruction from then/else last block to continue block *)
      let _ = build_br continue_bb (builder_at_end llctx then_end) in

      (* In current basic block, generate code for conditional jump *)
      gen_cond curr_basic_block true_bb continue_bb cond;
      continue_bb

    | AssignStat (var, exp) ->
      let (llexp, new_bb) = gen_exp curr_basic_block exp in
      let llvar = gen_addr curr_basic_block var in

      let builder = builder_at_end llctx new_bb in
      let _ = build_store llexp llvar builder in
      new_bb

    | PutStat exp ->
      let (llval, new_bb) = gen_exp curr_basic_block exp in
      let builder = builder_at_end llctx new_bb in
      let zero = const_int (i32_type llctx) 0 in

      let _ = (
        match exp.t with
        | Int ->
          let int_format_str = build_global_stringptr "%ld\n" "" builder in
          let fs = build_in_bounds_gep int_format_str [| zero |] "" builder in
          build_call printf [| fs; llval |] "" builder

        | Float ->
          let float_format_str = build_global_stringptr "%lf\n" "" builder in
          let fs = build_in_bounds_gep float_format_str [| zero |] "" builder in
          build_call printf [| fs; llval |] "" builder

        | _ -> raise (SurprisedPikachu "Put stat case not implemented yet")
      ) in
      new_bb

    | ReturnStat (Some exp) ->
      let (llexp, new_bb) = gen_exp curr_basic_block exp in
      let builder = builder_at_end llctx new_bb in
      let _ = build_ret llexp builder in
      new_bb

    | ReturnStat None ->
      let builder = builder_at_end llctx curr_basic_block in
      let _ = build_ret_void builder in
      curr_basic_block

    | _ -> raise (SurprisedPikachu "Stat case not implemented yet")

  and gen_block curr_func curr_basic_block env block_node =
    (* TODO: variable declarations *)

    let stat_acc (bb, env) stat =
      (gen_stat curr_func bb env stat, env)
    in

    List.fold_left block_node.statements ~init:(curr_basic_block, env) ~f:stat_acc
  in
  (* *)

  let acc_def env def =
    match def with
    | VarDef var ->
      let lltyp = lltype_from_mongatype var.t in
      let initial_val = const_null lltyp in
      let llval = define_global "" initial_val llmodule in
      NameEnv.add var.name llval env

    | FuncDef (name, ft, block) ->
      (* create function at module and add its name to env *)
      let llft = lltype_from_functype ft in
      let f = define_function name llft llmodule in
      let new_env = NameEnv.add name f env in
      let llblock = entry_block f in
      let llbuilder = builder_at_end llctx llblock in

      (* add parameters to env *)
      let (new_new_env, _) =
        List.fold_left ft.parameters ~init:(env, 0)  ~f:(
          fun (env, ll_ppos) m_param ->
            let llparam = param f ll_ppos in
            let v = build_alloca (lltype_from_mongatype m_param.t) "" llbuilder in
            let _ = build_store llparam v llbuilder in
            (NameEnv.add m_param.name v env, ll_ppos + 1)
        )
      in

      (* build function *)
      let _ = gen_block f llblock new_new_env block in


      new_env
  in

  let _ = List.fold_left typed_tree ~init:(NameEnv.empty) ~f:acc_def in

  llmodule
