open AstTypes
open Base

(* Typed Expression *)
type t_exp_node = {
  exp: t_exp;
  t: monga_type
}

and t_exp =
  | AddExp of t_exp_node * t_exp_node
  | SubExp of t_exp_node * t_exp_node
  | MulExp of t_exp_node * t_exp_node
  | DivExp of t_exp_node * t_exp_node
  | EqExp of t_exp_node * t_exp_node
  | NeExp of t_exp_node * t_exp_node
  | LeExp of t_exp_node * t_exp_node
  | GeExp of t_exp_node * t_exp_node
  | LtExp of t_exp_node * t_exp_node
  | GtExp of t_exp_node * t_exp_node
  | AndExp of t_exp_node * t_exp_node
  | OrExp of t_exp_node * t_exp_node
  | UnaryMinusExp of t_exp_node
  | UnaryNotExp of t_exp_node
  | TrueExp | FalseExp
  | FloatExp of float
  | IntExp of int
  | StringExp of string
  | NewExp of monga_type * t_exp_node
  | CastExp of t_exp_node * monga_type
  | LookupExp of t_exp_node * t_exp_node
  | VarExp of id
  | CallExp of id * t_exp_node list


(* Typed Block, Statements and Definitions *)
type t_def_node =
  | VarDef of monga_variable
  | FuncDef of id * monga_function_type * t_block_node

and t_block_node = {
  var_decs : monga_variable list;
  statements : t_stat_node list
}

and t_stat_node =
  | IfElseStat of t_exp_node * t_block_node * t_block_node option
  | WhileStat of t_exp_node * t_block_node
  | ReturnStat of t_exp_node option
  | AssignStat of t_exp_node * t_exp_node
  | CallStat of id * t_exp_node list
  | PutStat of t_exp_node
  | BlockStat of t_block_node


(* Typed AST *)
type typed_tree = t_def_node list



module TypeEnv = Stdlib.Map.Make(String)
type env_element =
  | Func of monga_function_type
  | Var of monga_type



let rec type_exp env exp =
  (* let (>>=) = Result.bind in *)
  let open Result in
  let arthm lhs rhs =
    type_exp env lhs >>= fun t_lhs ->
    type_exp env rhs >>= fun t_rhs ->

    match t_lhs.t, t_rhs.t with
    | Int, Int ->
      Ok (t_lhs, t_rhs, Int)

    | Float, Float ->
      Ok (t_lhs, t_rhs, Float)

    | Int, Float ->
      let cast_lhs = {
        exp = CastExp (t_lhs, Float);
        t = Float
      } in
      Ok (cast_lhs, t_rhs, Float)

    | Float, Int ->
      let cast_rhs = {
        exp = CastExp (t_rhs, Float);
        t = Float
      } in
      Ok (t_lhs, cast_rhs, Float)

    | _, _ ->
      Error "Type Error"
  in

  (* Equality doesn't promote int to float *)
  let eq_exp lhs rhs =
    type_exp env lhs >>= fun t_lhs ->
    type_exp env rhs >>= fun t_rhs ->
    if Poly.equal t_lhs.t t_rhs.t then
      Ok (t_lhs, t_rhs, Bool)
    else
      Error "Type Error"
  in

  let log_exp lhs rhs =
    type_exp env lhs >>= fun t_lhs ->
    type_exp env rhs >>= fun t_rhs ->
    match t_lhs.t, t_rhs.t with
    | Bool, Bool -> Ok (t_lhs, t_rhs, Bool)
    | _, _ ->  Error "Type Error"
  in


  match exp with
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


  (* Equality Exp *)
  | UntypedAst.EqExp (lhs, rhs) ->
    eq_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = EqExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.NeExp (lhs, rhs) ->
    eq_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = NeExp (t_lhs, t_rhs) in
    Ok {exp; t}


  (* Relational Exp, same semantics as arithm *)
  | UntypedAst.LeExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = LeExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.GeExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = GeExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.LtExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = LtExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.GtExp (lhs, rhs) ->
    arthm lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = GtExp (t_lhs, t_rhs) in
    Ok {exp; t}


  (* Logical Exp *)
  | UntypedAst.AndExp (lhs, rhs) ->
    log_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = AndExp (t_lhs, t_rhs) in
    Ok {exp; t}

  | UntypedAst.OrExp (lhs, rhs) ->
    log_exp lhs rhs >>= fun (t_lhs, t_rhs, t) ->
    let exp = AndExp (t_lhs, t_rhs) in
    Ok {exp; t}

  (* Unary *)
  | UntypedAst.UnaryMinusExp e ->
    type_exp env e >>= fun t_exp ->
    (match t_exp.t with
    | Float -> Ok {exp = UnaryMinusExp t_exp; t = Float}
    | Int -> Ok {exp = UnaryMinusExp t_exp; t = Int}
    | _ -> Error "Type Error")

  | UntypedAst.UnaryNotExp e ->
    type_exp env e >>= fun t_exp ->
    (match t_exp.t with
    | Bool -> Ok {exp = UnaryNotExp t_exp; t = Bool}
    | _ -> Error "Type Error")

  | UntypedAst.NewExp (mt, e) ->
    type_exp env e >>= fun t_exp ->
    (match t_exp.t with
    | Int -> Ok {exp = NewExp (mt, t_exp); t = Array mt}
    | _ -> Error "Type Error")

  | UntypedAst.CastExp (_, _) ->
    Error "CastExp"

  (* Named expressions *)
  | UntypedAst.VarExp name ->
    if TypeEnv.mem name env then
      (match TypeEnv.find name env with
       | Var mt -> Ok {exp = VarExp name; t = mt}
       | Func _ -> Error "Type Error")
    else
      Error "Type Error"

  | UntypedAst.LookupExp (e, idx) ->
    type_exp env e >>= fun t_exp ->
    type_exp env idx >>= fun t_idx ->
    (match t_exp.t, t_idx.t with
     | Array t, Int -> Ok {exp = LookupExp (t_exp, t_idx); t}
     | _, _ -> Error "Type Error")

  | UntypedAst.CallExp (name, args) ->
    if TypeEnv.mem name env then
      (match TypeEnv.find name env with

       (* Functions with no return type cannot be used in expressions *)
       | Func {parameters; ret_type = Some ret_type} ->

         (* Type all arguments first, getting a list of results *)
         let t_args_res = List.map ~f:(type_exp env) args in

         (match

            (* Combine list of results into a result that contains a list *)
            Result.combine_errors t_args_res >>= fun t_args ->

            (* Checks if arguments types match with parameter types.
               If there's a count mismatch, raises exception *)
            let exception WrongArgumentCount in
            let rec zip a (b : monga_variable list) =
              match a, b with
              | [], [] -> [Ok ()]
              | arg :: xs,  param :: ys ->
                let tail = zip xs ys in
                if Poly.equal arg.t param.t then
                  Ok () :: tail
                else
                  Error "" :: tail
              | _, _ ->
                raise WrongArgumentCount
            in

            try
              let res = Result.combine_errors (zip t_args parameters) in
              (match res with
               | Ok _ -> Ok t_args
               | Error x -> Error x)
            with
            | WrongArgumentCount -> Error [""]

          with
          | Ok t_args -> Ok {exp = CallExp (name, t_args); t = ret_type}
          | Error _ -> Error "")

       | Func { parameters = _ ; ret_type = None} -> Error ""
       | Var _ -> Error "Type Error")
    else
      Error "Type Error"



(*
let add_var var env =
  if (TypeEnv.mem var.name env) then
    (* Error, uses newest declaration of var and keeps typing *)
    let new_env = TypeEnv.add var.name (Var var.t) env in
    let error_msg = "Redeclared name" in
    Error (error_msg, new_env)
  else
    let new_env = TypeEnv.add var.name (Var var.t) env in
    Ok (new_env)

let compose_error error = function
  | Ok (new_env) ->
    Error (error, new_env)

  | Error (new_error, new_env) ->
    let error_msg = error ^ new_error in
    Error (error_msg, new_env)

let rec compose_env env_res = function
  | [] -> env_res
  | v :: vs ->
    match env_res with
    | Ok old_env ->
      let res = add_var v old_env in
      compose_env res vs

    | Error (old_error, old_env) ->
      let res = add_var v old_env in
      let new_res = compose_env res vs in
      compose_error old_error new_res

let rec build_block env_res curr_func (block : UntypedAst.block_node) =
  let block_env = compose_env env_res block.var_decs in

  let rec compose_stat = function
    | [] -> Ok []
    | x :: xs ->
      let stat_res = build_statement block_env curr_func x in
      let stats_res = compose_stat xs in
      match stat_res, stats_res with
      | Ok t_stat, Ok t_stats ->
        Ok (t_stat :: t_stats)

      | Ok t_stat, Error (msg, t_stats) ->
        Error (msg, t_stat :: t_stats)

      | Error (msg, t_stat), Ok t_stats ->
        Error (msg, t_stat :: t_stats)

      | Error (msg1, t_stat), Error (msg2, t_stats) ->
        let error_msg = msg1 ^ msg2 in
        Error (error_msg, t_stat :: t_stats)
  in

  let t_stats_res = compose_stat block.statements in
  match t_stats_res with
  | Ok t_stats -> Ok {var_decs = block.var_decs; statements = t_stats}
  | Error (msg, t_stats) -> Error (msg, {var_decs = block.var_decs; statements = t_stats})

and build_statement env_res curr_func = function
  | UntypedAst.IfElseStat (condition, then_block, None) ->
    let t_cond = type_exp env_res condition in
    let t_then = build_block env_res curr_func then_block in
    Ok(IfElseStat (t_cond, t_then, None))

  | UntypedAst.IfElseStat (condition, then_block, Some else_block) ->
    let t_cond = type_exp env_res condition in
    let t_then = build_block env_res curr_func then_block in
    let t_else = build_block env_res curr_func else_block in
    Ok(IfElseStat (t_cond, t_then, Some t_else))

  | UntypedAst.WhileStat (condition, block) ->
    let t_exp = type_exp env_res condition in
    let t_block = build_block env_res curr_func block in
    Ok(WhileStat (t_exp, t_block))

  | UntypedAst.ReturnStat (None) -> 
    ()

  | UntypedAst.ReturnStat (Some exp) -> 
    ()

  | UntypedAst.AssignStat (var, exp) ->
    ()

  | UntypedAst.CallStat (func, args) ->
    ()

  | UntypedAst.PutStat exp ->
    ()

  | UntypedAst.BlockStat block ->
    ()


let build_def def env =
  match def with
  | UntypedAst.VarDef var ->
    add_var var env

  | UntypedAst.FuncDef (name, func_type, block) ->
    if (TypeEnv.mem name env) then
      (* Error, uses newest declaration of var and keeps typing *)
      let new_env = TypeEnv.add name (Func func_type) env in
      let error_msg = "Redeclared name" in
      Error (error_msg, new_env)
    else
      Ok (env)
*)
