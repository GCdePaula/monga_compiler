open Base
open TypedAst
open AstTypes
open Llvm

exception FatalGenError of string

module NameEnv = Stdlib.Map.Make(String)

let gen_code typed_tree =

  let llctx = global_context () in
  let llmodule = create_module llctx "program" in

  (* Helper*)

  (* "Runtime" constants *)
  let llprintf =
    let i8_t = i8_type llctx in
    let i32_t = i32_type llctx in
    let print_type = var_arg_function_type i32_t [| pointer_type i8_t |] in
    declare_function "printf" print_type llmodule
  in

  let llmalloc =
    let i8p_t = pointer_type (i8_type llctx) in
    let i64_t = i64_type llctx in
    let malloc_type = function_type i8p_t [| i64_t |] in
    declare_function "malloc" malloc_type llmodule
  in

  (*
  let llstdout_type = pointer_type (named_struct_type llctx "struct._IO_FILE") in
  let llfflush =
    let fflush_type = function_type (i32_type llctx) [| llstdout_type |] in
    declare_function "fflush" fflush_type llmodule
  in
  *)

  (* *)

  (* Helper functions *)
  let rec lltype_from_mongatype m_type =
    match m_type with
    | Int -> i64_type llctx
    | Float -> double_type llctx
    | Bool -> i1_type llctx
    | Char -> i8_type llctx
    | Array x -> pointer_type (lltype_from_mongatype x)
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

  let get_builder bb = builder_at_end llctx bb in

  let str_constant_table = Hashtbl.create ~growth_allowed:true ~size:8 (module String) in
  let get_llstr str builder =
    Hashtbl.find_or_add str_constant_table str ~default: (
        fun () -> build_global_stringptr str "" builder
      )
  in

  (* *)

  (* Actual code generation *)
  let rec gen_stat curr_func start_basic_block env stat_node =

    (* Adds inconditional jump, but only if there isn't a terminator already. 
     * This isn't ideal. *)
    let build_jmp from_bb to_bb =
      match block_terminator from_bb with
      | None ->
        let _ = build_br to_bb (get_builder from_bb) in
        ()
      | Some _ -> ()
    in

    (* Generates the value of given expressions, returning list of llvalue and llbasicblock *)
    let rec gen_args bb exp_list =
      match exp_list with
      | [] -> ([], bb)
      | exp :: exps ->
        let (llexp, new_bb) = gen_exp bb exp in
        let (llexps, last_bb) = (gen_args new_bb exps) in
        (llexp :: llexps, last_bb)

    (* Generates the value of fiven expression, returning llvalue and llbasicblock *)
    and gen_exp curr_bb exp_node =
      let build_bin_exp (exp1: t_exp_node) exp2 build_iop build_fop =
        let (llexp1, new_bb1) = gen_exp curr_bb exp1 in
        let (llexp2, new_bb2) = gen_exp new_bb1 exp2 in

        let builder = get_builder new_bb2 in
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

      | StringExp str ->
        let builder = get_builder curr_bb in
        let llstr = get_llstr str builder in
        (llstr, curr_bb)

      | VarExp name ->
        let llvar = NameEnv.find name env in
        let builder = get_builder curr_bb in
        let llvarexp = build_load llvar "" builder in
        (llvarexp, curr_bb)

      | LookupExp (arr, idx)->
        let (llarr, new_bb1) = gen_exp curr_bb arr in
        let (llidx, new_bb2) = gen_exp new_bb1 idx in

        let builder = get_builder new_bb2 in
        let lladd = build_gep llarr [|llidx|] "" builder in
        let llres = build_load lladd "" builder in
        (llres, new_bb2)

      | CallExp (name, args) ->
        let (llargs, new_bb) = gen_args curr_bb args in
        let builder = get_builder new_bb in
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
        let _ = build_br continue_bb (get_builder true_bb) in
        let _ = build_br continue_bb (get_builder false_bb) in

        (* The value of the expression is given by where it came from *)
        let lltrue = const_int (lltype_from_mongatype Bool) 1 in
        let llfalse = const_int (lltype_from_mongatype Bool) 0 in
        let builder = get_builder continue_bb in
        let incoming = [(lltrue, true_bb); (llfalse, false_bb)] in
        let result = build_phi incoming "" builder in
        (result, continue_bb)

      | UnaryNotExp exp ->
        let (llval, new_bb) = gen_exp curr_bb exp in
        let builder = get_builder new_bb in
        let result = build_not llval "" builder in
        (result, new_bb)

      | UnaryMinusExp exp ->
        let (llval, new_bb) = gen_exp curr_bb exp in
        let builder = get_builder new_bb in
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

      | NewExp (t, size) ->
        let (lllength, new_bb) = gen_exp curr_bb size in
        let builder = get_builder new_bb in
        let lltype = lltype_from_mongatype t in
        let llptrtype = pointer_type lltype in
        let llsize = build_mul lllength (size_of lltype) "" builder in
        let lladdr = build_call llmalloc [|llsize|] "" builder in
        let res = build_pointercast lladdr llptrtype "" builder in
        (res, new_bb)

      | CastExp (exp, mt) ->
        let (llval, new_bb) = gen_exp curr_bb exp in
        let builder = get_builder new_bb in
        let llres =
          match exp.t, mt with
          | Bool, Int | Bool, Char ->
            build_zext llval (lltype_from_mongatype mt) "" builder
          | Bool, Float ->
            build_uitofp llval (lltype_from_mongatype mt) "" builder
          | Char, Int | Int, Char ->
            build_intcast llval (lltype_from_mongatype mt) "" builder
          | Int, Float | Char, Float ->
            build_sitofp llval (lltype_from_mongatype mt) "" builder
          | Float, Int | Float, Char ->
            build_fptosi llval (lltype_from_mongatype mt) "" builder
          | _ ->
            raise (FatalGenError "Unreachable Case: exp cast case not allowed")
        in
        (llres, new_bb)

    and gen_cond curr_bb true_bb false_bb cond_node =
      let build_cmp exp1 exp2 icmp fcmp =
        let (llexp1, new_bb1) = gen_exp curr_bb exp1 in
        let (llexp2, new_bb2) = gen_exp new_bb1 exp2 in
        let builder = get_builder new_bb2 in
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

        let builder = get_builder new_bb in
        let _ = build_cond_br llval true_bb false_bb builder in
        ()
    in

    (* Gets the address of given variable *)
    let gen_addr bb var_node =
      match var_node with
      | SimpleVar var ->
        (NameEnv.find var.name env, bb)

      | LookupVar (arr, idx) ->
        let (llarr, new_bb1) = gen_exp bb arr in
        let (llidx, new_bb2) = gen_exp new_bb1 idx in

        let builder = get_builder new_bb2 in
        let llres = build_gep llarr [|llidx|] "" builder in
        (llres, new_bb2)
    in


    match stat_node with
    | AssignStat (var, exp) ->
      let (llexp, new_bb1) = gen_exp start_basic_block exp in
      let (llvar, new_bb2) = gen_addr new_bb1 var in

      let builder = get_builder new_bb2 in
      let _ = build_store llexp llvar builder in
      new_bb2

    | CallStat (name, args) ->
      let (llargs, new_bb) = gen_args start_basic_block args in
      let builder = get_builder new_bb in
      let llf = NameEnv.find name env in
      let _ = build_call llf (Array.of_list llargs) "" builder in
      new_bb

    | IfElseStat (cond, then_block, Some else_block) ->
      (* Creates basic blocks for then, else, and the following block *)
      let true_bb = append_block llctx "" curr_func in
      let false_bb = append_block llctx "" curr_func in

      (* Generates code for then block and else block*)
      let then_end = gen_block curr_func true_bb env then_block in
      let else_end = gen_block curr_func false_bb env else_block in

      (* Adds branch instruction from then/else last block to continue block *)
      let continue_bb = append_block llctx "" curr_func in
      let _ = build_jmp then_end continue_bb in
      let _ = build_jmp else_end continue_bb in

      (* In current basic block, generate code for conditional jump *)
      gen_cond start_basic_block true_bb false_bb cond;
      continue_bb

    | IfElseStat (cond, then_block, None) ->
      (* Creates basic blocks for then, else, and the following block *)
      let true_bb = append_block llctx "" curr_func in

      (* Generates code for then block and else block*)
      let then_end = gen_block curr_func true_bb env then_block in

      (* Adds branch instruction from then/else last block to continue block *)
      let continue_bb = append_block llctx "" curr_func in
      let _ = build_jmp then_end continue_bb in

      (* In current basic block, generate code for conditional jump *)
      gen_cond start_basic_block true_bb continue_bb cond;
      continue_bb

    | WhileStat (cond, while_block) ->
      (* Create condition basic block, and add branch from start bb to it *)
      let cond_bb = append_block llctx "" curr_func in
      let _ = build_br cond_bb (get_builder start_basic_block) in

      (* Create loop basic block, generate code, and loop *)
      let while_start_bb = append_block llctx "" curr_func in
      let while_end_bb = gen_block curr_func while_start_bb env while_block in
      let _ = build_br cond_bb (get_builder while_end_bb) in

      (* Create continuation basic block, and add conditional branch *)
      let continue_bb = append_block llctx "" curr_func in
      gen_cond cond_bb while_start_bb continue_bb cond;
      continue_bb

    | BlockStat block ->
      let new_bb = gen_block curr_func start_basic_block env block in
      new_bb

    | PutStat exp ->
      let (llval, new_bb) = gen_exp start_basic_block exp in
      let builder = get_builder new_bb in

      let print_format format_str =
        let zero = const_int (i32_type llctx) 0 in
        let llformat_str = get_llstr format_str builder in
        let fs = build_in_bounds_gep llformat_str [| zero |] "" builder in
        let _ = build_call llprintf [| fs; llval |] "" builder in
        (* let _ = build_call llfflush [|const_null llstdout_type |] "" builder in *)
        ()
      in

      let _ = (
        match exp.t with
        | Int -> print_format "%ld\n"
        | Float -> print_format "%lf\n"
        | Char -> print_format "'%c'\n"
        | Array (Char) -> print_format "\"%s\"\n"
        | Bool -> print_format "Bool: %d\n"
        | _ -> raise (FatalGenError "Unreachable Case: Put stat expression type not allowed")
      ) in
      new_bb

    | ReturnStat (Some exp) ->
      let (llexp, new_bb) = gen_exp start_basic_block exp in
      let builder = get_builder new_bb in
      let _ = build_ret llexp builder in
      new_bb

    | ReturnStat None ->
      let builder = get_builder start_basic_block in
      let _ = build_ret_void builder in
      start_basic_block

  and gen_block curr_func start_basic_block env block_node =
    let new_env =
      let builder = get_builder start_basic_block in
      let rec build_var_decs env var_list =
        match var_list with
        | [] -> env
        | var :: vars ->
          let llt = (lltype_from_mongatype var.t) in
          let llv = build_alloca (lltype_from_mongatype var.t) "" builder in
          let _ = build_store (const_null llt) llv builder in
          let new_env = NameEnv.add var.name llv env in
          build_var_decs new_env vars
      in
      build_var_decs env block_node.var_decs
    in

    let new_bb =
      let rec build_stats bb stat_list =
        match stat_list with
        | [] -> bb
        | stat :: stats ->
          let new_bb = gen_stat curr_func bb new_env stat in
          build_stats new_bb stats
      in
      build_stats start_basic_block block_node.statements
    in
    new_bb
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
      (* Create function at module and add its name to env *)
      let llft = lltype_from_functype ft in
      let f = define_function name llft llmodule in
      let new_env = NameEnv.add name f env in
      let llblock = entry_block f in
      let llbuilder = get_builder llblock in

      (* Add parameters to env *)
      let (new_new_env, _) =
        List.fold_left ft.parameters ~init:(new_env, 0)  ~f:(
          fun (env, ll_ppos) m_param ->
            let llparam = param f ll_ppos in
            let v = build_alloca (lltype_from_mongatype m_param.t) "" llbuilder in
            let _ = build_store llparam v llbuilder in
            (NameEnv.add m_param.name v env, ll_ppos + 1)
        )
      in

      (* build function *)
      let _ = gen_block f llblock new_new_env block in
      new_new_env
  in

  let _ = List.fold_left typed_tree ~init:(NameEnv.empty) ~f:acc_def in

  llmodule
