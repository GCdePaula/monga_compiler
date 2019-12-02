open Base
open AstTypes
open TypedAst


module NameEnv = Stdlib.Map.Make(String)
type env_element =
  | Func of monga_function_type
  | Var of monga_type

let combine_res x_res y_res f_ok =
    Result.combine x_res y_res ~ok:f_ok ~err: List.append


(* Type expresions and function *)
let rec type_exp env (exp_node: UntypedAst.exp_node) =
  let exception ArthmError of (location * monga_type) list in
  let exception PromoteError of string in (* Internal error, should not happen *)

  let is_arthm = function Float | Int | Char -> true | _ -> false in

  let promote_to_float loc exp_t =
    match exp_t.t with
    | Float -> exp_t
    | Int -> {exp = CastExp (exp_t, Float); t = Float}
    | Char -> {exp = CastExp (exp_t, Float); t = Float}
    | x -> raise (ArthmError [(loc, x)])
  in

  let promote_to_int loc exp_t =
    match exp_t.t with
    | Float -> raise (PromoteError "Cannot promote float to int")
    | Int -> exp_t
    | Char -> {exp = CastExp (exp_t, Int); t = Int}
    | x -> raise (ArthmError [(loc, x)])
  in

  let arthm lhs rhs =
    let t_lhs_res = type_exp env lhs in
    let t_rhs_res = type_exp env rhs in

    try
      combine_res t_lhs_res t_rhs_res (
        fun t_lhs t_rhs ->
          match t_lhs.t, t_rhs.t with
          | Float, _ ->
            (t_lhs, promote_to_float rhs.loc t_rhs, Float)

          | _, Float ->
            (promote_to_float lhs.loc t_lhs, t_rhs, Float)

          | Int, _ ->
            (t_lhs, promote_to_int rhs.loc t_rhs, Int)

          | _, Int ->
            (promote_to_int lhs.loc t_lhs, t_rhs, Int)

          | Char, Char ->
            (t_lhs, t_rhs, Char)

          | x, y ->
            let err = ref [] in
            err := if is_arthm y then !err else (rhs.loc, y) :: !err;
            err := if is_arthm x then !err else (lhs.loc, x) :: !err;
            raise (ArthmError !err)
      )
    with
    | ArthmError xs -> Error (List.map xs ~f:(fun x -> {loc=fst x; err=NotArithmeticTypeError (snd x)}))
  in

  (* Relational only works on arthm types, promoting expressions if necessary *)
  let relational_exp lhs rhs =
    let t_lhs_res = type_exp env lhs in
    let t_rhs_res = type_exp env rhs in

    try
      combine_res t_lhs_res t_rhs_res (
        fun t_lhs t_rhs ->
          match t_lhs.t, t_rhs.t with
          | Float, _ ->
            (t_lhs, promote_to_float rhs.loc t_rhs, Bool)

          | _, Float ->
            (promote_to_float lhs.loc t_lhs, t_rhs, Bool)

          | Int, _ ->
            (t_lhs, promote_to_int rhs.loc t_rhs, Bool)

          | _, Int ->
            (promote_to_int lhs.loc t_lhs, t_rhs, Bool)

          | Char, Char ->
            (t_lhs, t_rhs, Bool)

          | x, y ->
            let err = ref [] in
            err := if is_arthm y then !err else (rhs.loc, y) :: !err;
            err := if is_arthm x then !err else (lhs.loc, x) :: !err;
            raise (ArthmError !err)
      )
    with
    | ArthmError xs -> Error (List.map xs ~f:(fun x -> {loc=fst x; err=NotArithmeticTypeError (snd x)}))
  in

  (* Equality operators only work between arthm types (promoting if necessary), otherwise on same type *)
  let equality_exp lhs rhs =
    let t_lhs_res = type_exp env lhs in
    let t_rhs_res = type_exp env rhs in

    let exception EqError of monga_type * monga_type in
    try
      combine_res t_lhs_res t_rhs_res (
        fun t_lhs t_rhs ->
          match t_lhs.t, t_rhs.t with
          | Float, _ ->
            (t_lhs, promote_to_float rhs.loc t_rhs, Bool)

          | _, Float ->
            (promote_to_float lhs.loc t_lhs, t_rhs, Bool)

          | Int, _ ->
            (t_lhs, promote_to_int rhs.loc t_rhs, Bool)

          | _, Int ->
            (promote_to_int lhs.loc t_lhs, t_rhs, Bool)

          | Char, Char ->
            (t_lhs, t_rhs, Bool)

          | x, y ->
            if Poly.equal x y then
              (t_lhs, t_rhs, Bool)
            else
              raise (EqError (t_lhs.t, t_rhs.t))
      )
    with
    | ArthmError xs -> Error (List.map xs ~f:(fun x -> {loc=fst x; err=NotArithmeticTypeError (snd x)}))
    | EqError (l_type, r_type) -> Error [{loc=((fst lhs.loc), (snd rhs.loc)); err=IncompatibleTypeError (r_type, l_type)}]
  in

  (* Only works between bool types *)
  let logic_exp lhs rhs =
    let t_lhs_res = type_exp env lhs in
    let t_rhs_res = type_exp env rhs in

    let exception LogicError of ((Lexing.position * Lexing.position) * monga_type) list in
    try
      combine_res t_lhs_res t_rhs_res (
        fun t_lhs t_rhs ->
          match t_lhs.t, t_rhs.t with
          | Bool, Bool -> (t_lhs, t_rhs, Bool)
          | x, y ->
            let err = ref [] in
            err := if Poly.equal y Bool then !err else (rhs.loc, y) :: !err;
            err := if Poly.equal x Bool then !err else (lhs.loc, x) :: !err;
            raise (LogicError !err)
      )
    with
    | LogicError xs -> Error (List.map xs ~f:(fun x -> {loc=fst x; err=IncompatibleTypeError (snd x, Bool)}))
  in

  let open Result in
  match exp_node.exp with

  (* base cases *)
  | UntypedAst.TrueExp ->
    Ok {exp = TrueExp; t = Bool}

  | UntypedAst.FalseExp ->
    Ok {exp = FalseExp; t = Bool}

  | UntypedAst.FloatExp fnum ->
    Ok {exp = FloatExp fnum; t = Float}

  | UntypedAst.IntExp inum ->
    Ok {exp = IntExp inum; t = Int}

  | UntypedAst.StringExp str ->
    Ok {exp = StringExp str; t = Array Char}

  (* Arithmetic bin exp *)
  | UntypedAst.AddExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = AddExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.SubExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = SubExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.MulExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = MulExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.DivExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = DivExp (t_lhs, t_rhs) in
    Ok {exp; t}

  (* Relational Exp *)
  | UntypedAst.EqExp (lhs, rhs) ->
    equality_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = EqExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.NeExp (lhs, rhs) ->
    equality_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = NeExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.LeExp (lhs, rhs) ->
    relational_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = LeExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.GeExp (lhs, rhs) ->
    relational_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = GeExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.LtExp (lhs, rhs) ->
    relational_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = LtExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.GtExp (lhs, rhs) ->
    relational_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = GtExp (t_lhs, t_rhs) in
    Ok {exp; t}


  (* Logical Exp *)
  | UntypedAst.AndExp (lhs, rhs) ->
    logic_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = AndExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.OrExp (lhs, rhs) ->
    logic_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = AndExp (t_lhs, t_rhs) in
    Ok {exp; t}

  (* Unary *)
  | UntypedAst.UnaryMinusExp e ->
    type_exp env e >>= fun t_exp ->
    (match t_exp.t with
    | Float -> Ok {exp = UnaryMinusExp t_exp; t = Float}
    | Int -> Ok {exp = UnaryMinusExp t_exp; t = Int}
    | _ -> Error [{loc=e.loc; err=NotArithmeticTypeError t_exp.t}])

  | UntypedAst.UnaryNotExp e ->
    type_exp env e >>= fun t_exp ->
    (match t_exp.t with
    | Bool -> Ok {exp = UnaryNotExp t_exp; t = Bool}
    | _ -> Error [{loc=e.loc; err=IncompatibleTypeError (t_exp.t, Bool)}])

  | UntypedAst.NewExp (mt, e) ->
    type_exp env e >>= fun t_exp ->
    (match t_exp.t with
    | Int -> Ok {exp = NewExp (mt, t_exp); t = Array mt}
    | _ -> Error [{loc=e.loc; err=IncompatibleTypeError (t_exp.t, Int)}])

  (* Cast *)
  | UntypedAst.CastExp (e, mt) ->
    type_exp env e >>= fun t_exp ->
    (match t_exp.t, mt with
     | x, y when (is_arthm x) && (is_arthm y) ->
       Ok {exp = CastExp (t_exp, mt); t = mt}
     | Bool, x when (is_arthm x) ->
       Ok {exp = CastExp (t_exp, mt); t = mt}
     | _ ->
       Error [{loc=e.loc; err=InvalidCast (t_exp.t, mt)}])

  (* Named expressions *)
  | UntypedAst.VarExp name ->
    (try
      (match NameEnv.find name env with
       | Var mt -> Ok {exp = VarExp name; t = mt}
       | Func _ -> Error [{loc=exp_node.loc; err=NotAVar name}])
    with
    | Caml.Not_found -> Error [{loc=exp_node.loc; err=UnboundName name}])

  | UntypedAst.LookupExp (e, idx) ->
    type_exp env e >>= fun t_exp ->
    type_exp env idx >>= fun t_idx ->
    (match t_exp.t, t_idx.t with
     | Array t, Int -> Ok {exp = LookupExp (t_exp, t_idx); t}
     | _, _ -> Error [{loc=((fst e.loc), (snd idx.loc)); err=IndexTypeError (t_exp.t, t_idx.t)}])

  | UntypedAst.CallExp (name, args) ->
    try
      (match NameEnv.find name env with

       (* Only functions with return type can be used in expressions *)
       | Func {parameters; ret_type = Some ret_type} ->
         type_func exp_node.loc env args parameters >>= fun t_args ->
         Ok {exp = CallExp (name, t_args); t = ret_type}

       | Func { parameters = _ ; ret_type = None} -> Error [{loc=exp_node.loc; err=IncompatibleRetType}]
       | Var _ -> Error [{loc=exp_node.loc; err=NotAFunc name}])
    with
    | Caml.Not_found -> Error [{loc=exp_node.loc; err=UnboundName name}]

and type_func loc env args parameters =
  let open Result in

  (* Type all arguments first, getting a list of results.
   * Each member is either Ok of a typed expression, or
   * Error of a list of error types.
  *)
  let t_args_res = List.map ~f:(type_exp env) args in

  (* Combine list of results into a result that contains a list.
   * The type of result's error case is a list of list of errors.
   * The map_error flattens that list of lists.*)
  map_error (
    combine_errors t_args_res >>= fun t_args ->

    (* Checks if arguments types match with parameter types.
     * If there's a count mismatch, raises exception *)
    let exception WrongArgumentCount of int * int in
    let rec zip a (b : monga_variable list) count =
      match a, b with
      | [], [] -> [Ok ()]
      | arg :: xs,  param :: ys ->
        let tail = zip xs ys (count+1) in
        if Poly.equal arg.t param.t then
          Ok () :: tail
        else (* loc is not accurate *)
          Error ({loc=loc; err=IncompatibleTypeError (arg.t, param.t)}) :: tail

      | x, [] ->
        raise (WrongArgumentCount (count + List.length x, count))
      | [], y  ->
        raise (WrongArgumentCount (count, count + List.length y))
    in

    try
      let res = combine_errors (zip t_args parameters 0) in
      (match res with
       | Ok _ -> Ok t_args
       | Error x -> Error [x])
    with
    | WrongArgumentCount (got, want)-> Error [[{loc=loc; err=WrongNumberOfArgs (got, want)}]]
  ) ~f:List.concat



(* Type block and statement *)
let rec build_block env curr_func (block : UntypedAst.block_node) : (TypedAst.t_block_node, TypedAst.error list) Result.t =
  let compose_env env var_decs =
    let acc_var (env, err_list) (loc, var) =
      if (NameEnv.mem var.name env) then
        (* Error, uses newest declaration of var *)
        let new_env = NameEnv.add var.name (Var var.t) env in
        (new_env, {loc=loc; err=RedeclaredName var.name} :: err_list)
      else
        let new_env = NameEnv.add var.name (Var var.t) env in
        (new_env, err_list)
    in

    let (new_env, err_list) =
      (List.fold_left var_decs ~init:(NameEnv.empty, []) ~f:acc_var)
    in

    (* List is built in reverse, so we correct it *)
    (NameEnv.union (fun _ _ elem -> Some elem) env new_env, List.rev err_list)
  in

  let (new_env, err_list) = compose_env env block.var_decs in
  let stats_res =
    List.map block.statements ~f:(build_statement new_env curr_func)
  in

  match Result.combine_errors stats_res with
  | Ok t_stats ->
    (match err_list with
    | [] -> Ok {var_decs = List.map block.var_decs ~f:snd; statements = t_stats}
    | x -> Error x)
  | Error x -> Error (err_list @ (List.concat x))


and build_statement env curr_func stat : (TypedAst.t_stat_node, TypedAst.error list) Result.t =
  let open Result in

  match stat.stat with
  | UntypedAst.IfElseStat (condition, then_block, Some else_block) ->
    let t_cond_res = type_exp env condition >>=  fun t_exp ->
      if Poly.equal t_exp.t Bool then
        Ok t_exp
      else
        Error [{loc=condition.loc; err=IncompatibleTypeError (t_exp.t, Bool)}]
    in
    let t_then_res = build_block env curr_func then_block in
    let t_else_res = build_block env curr_func else_block in

    let partial_res = combine_res t_cond_res t_then_res (
        fun t_cond t_then ->
          (t_cond, t_then)
      )
    in
    combine_res partial_res t_else_res (
      fun (t_cond, t_then) t_else ->
        IfElseStat (t_cond, t_then, Some t_else)
    )

  | UntypedAst.IfElseStat (condition, then_block, None) ->
    let t_cond_res = type_exp env condition >>=  fun t_exp ->
      if Poly.equal t_exp.t Bool then
        Ok t_exp
      else
        Error [{loc=condition.loc; err=IncompatibleTypeError (t_exp.t, Bool)}]
    in
    let t_then_res = build_block env curr_func then_block in
    combine_res t_cond_res t_then_res (
      fun t_cond t_then ->
        IfElseStat (t_cond, t_then, None)
    )

  | UntypedAst.WhileStat (condition, block) ->
    let t_cond_res = type_exp env condition >>=  fun t_exp ->
      if Poly.equal t_exp.t Bool then
        Ok t_exp
      else
        Error [{loc=condition.loc; err=IncompatibleTypeError (t_exp.t, Bool)}]
    in
    let t_block_res = build_block env curr_func block in
    combine_res t_cond_res t_block_res (
      fun t_cond t_block ->
        WhileStat (t_cond, t_block)
    )

  | UntypedAst.ReturnStat (exp_opt) ->
    (match curr_func.ret_type, exp_opt with
     | None, None ->
       Ok (ReturnStat None)

    | Some ret_type, Some exp ->
      type_exp env exp >>= fun t_exp ->
        if Poly.equal ret_type t_exp.t then
          Ok (ReturnStat (Some t_exp))
        else
          Error [{loc=exp.loc; err=IncompatibleTypeError (t_exp.t, ret_type)}]
    | _, _ -> Error [{loc=stat.loc; err=IncompatibleRetType}])

  | UntypedAst.AssignStat (var, exp) ->
    type_exp env var >>= fun t_var ->
    type_exp env exp >>= fun t_exp ->

    (
      match t_var.exp with
      | VarExp name ->
        if Poly.equal t_var.t t_exp.t then
          let var_node = SimpleVar ({name=name; t=t_var.t}) in
          Ok (AssignStat (var_node, t_exp))
        else
          Error [{loc=exp.loc; err=IncompatibleTypeError (t_exp.t, t_var.t)}]

      | LookupExp (arr, idx) ->
        if Poly.equal t_var.t t_exp.t then
          let var_node = LookupVar (arr, idx) in
          Ok (AssignStat (var_node, t_exp))
        else
          Error [{loc=exp.loc; err=IncompatibleTypeError (t_exp.t, t_var.t)}]

      | _ -> Error [{loc=var.loc; err=NotAssignable}]
    )

  | UntypedAst.CallStat (f_name, args) ->
    (try
      (match NameEnv.find f_name env with
       | Func {parameters; ret_type = _} ->
         type_func stat.loc env args parameters >>= fun t_args ->
         Ok (CallStat (f_name, t_args))

       | Var _ -> Error [{loc=stat.loc; err=NotAFunc f_name}]
      )
   with
   | Caml.Not_found -> Error [{loc=stat.loc; err=UnboundName f_name}])

  | UntypedAst.PutStat exp ->
    type_exp env exp >>= fun t_exp ->
    Ok (PutStat t_exp)

  | UntypedAst.BlockStat block ->
    build_block env curr_func block >>= fun t_block ->
    Ok (BlockStat t_block)


(* Type untyped tree *)
let build_typed_tree u_tree : (typed_tree, error list) Result.t =
  let acc_def (env, t_defs_res) def =
    match def with
    | UntypedAst.VarDef (loc, var) ->
      if NameEnv.mem var.name env then
        (* Error, uses newest declaration of var and keeps typing *)
        let new_env = NameEnv.add var.name (Var var.t) env in
        (new_env, Error [{loc=loc; err=RedeclaredName var.name}] :: t_defs_res)
      else
        let new_env = NameEnv.add var.name (Var var.t) env in
        (new_env, Ok (VarDef var) :: t_defs_res)

    | UntypedAst.FuncDef (loc, name, func_type, block) ->
      let params = List.map func_type.parameters ~f:(fun x -> (loc, x)) in
      let new_block = {block with var_decs = params @ block.var_decs} in
      if NameEnv.mem name env then
        (* Error, uses newest declaration of func and keeps typing *)
        let new_env = NameEnv.add name (Func func_type) env in

        match build_block new_env func_type new_block with
        | Error x ->
          (new_env, Error x :: Error [{loc=loc; err=RedeclaredName name}] :: t_defs_res)
        | _ ->
          (new_env, Error [{loc=loc; err=RedeclaredName name}] :: t_defs_res)

      else
        let new_env = NameEnv.add name (Func func_type) env in

        match build_block new_env func_type new_block with
        | Error x ->
          (new_env, Error x :: t_defs_res)
        | Ok t_block ->
          (new_env, Ok (FuncDef (name, func_type, t_block)) :: t_defs_res)
  in

  let (_, results) = List.fold_left u_tree ~init:(NameEnv.empty, []) ~f:acc_def in

  (Result.combine_errors results) |>
  (Result.map ~f:List.rev) |>
  (Result.map_error ~f:(fun err_list_list -> List.rev err_list_list |> List.concat ))

