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

  let int_format_str = define_global "" (const_stringz llctx "%ld\n") llmodule in
  let float_format_str = define_global "" (const_stringz llctx "%lf\n") llmodule in
  (**)

  (* Helper functions *)
  let lltype_from_mongatype m_type = (* TODO complete this function. Ask what type is array*)
    match m_type with
    | Int -> i64_type llctx
    | Float -> double_type llctx
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
  (**)

  (* Actual code generation *)
  let rec gen_stat llbuilder env stat_node =
    let rec gen_exp llbuilder env exp_node =
      match exp_node.exp with
      | VarExp name ->
        let llval = NameEnv.find name env in
        build_load llval "" llbuilder

      | IntExp inum ->
        const_int (lltype_from_mongatype Int) inum

      | FloatExp fnum ->
        const_float (lltype_from_mongatype Float) fnum

      | _ -> raise (SurprisedPikachu "Exp case not implemented yet")
    in

    let rec gen_addr llbuilder env exp_node =
      match exp_node.exp with
      | VarExp name ->
        NameEnv.find name env
      | _ -> raise (SurprisedPikachu "gen_addr case not implemented yet")
    in

    match stat_node with
    | AssignStat (var, exp) ->
      let llexp = gen_exp llbuilder env exp in
      let llvar = gen_addr llbuilder env var in
      let _ = build_store llexp llvar llbuilder in
      llbuilder

    | PutStat exp ->
      let llval = gen_exp llbuilder env exp in
      let _ = (
        match exp.t with
        | Int ->
          build_call printf [| int_format_str; llval |] "" llbuilder

        | Float ->
          build_call printf [| float_format_str; llval |] "" llbuilder

        | _ -> raise (SurprisedPikachu "Put stat case not implemented yet")
      ) in
      llbuilder

    | ReturnStat (Some exp) ->
      let llexp = gen_exp llbuilder env exp in
      let _ = build_ret llexp llbuilder in
      llbuilder

    | ReturnStat None ->
      let _ = build_ret_void llbuilder in
      llbuilder

    | _ -> raise (SurprisedPikachu "Stat case not implemented yet")

  and gen_block llbuilder env block_node =
    (* TODO: variable declarations *)

    let stat_acc (llbuilder, env) stat =
      (gen_stat llbuilder env stat, env)
    in

    List.fold_left block_node.statements ~init:(llbuilder, env) ~f:stat_acc
  in

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
      let llbuilder = builder_at_end llctx (entry_block f) in

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
      let _ = gen_block llbuilder new_new_env block in


      new_env
  in

  let _ = List.fold_left typed_tree ~init:(NameEnv.empty) ~f:acc_def in

  llmodule
