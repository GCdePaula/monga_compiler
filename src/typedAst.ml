open AstTypes

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




module TypeEnv = Map.Make(String)
type env_element =
  | Func of monga_function_type
  | Var of monga_type


let rec type_exp env exp =

  let arthm lhs rhs =
    let open Base.Result in
    type_exp env lhs >>= fun t_lhs ->
    type_exp env rhs >>= fun t_rhs ->

    (match t_lhs.t, t_rhs.t with
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
    )
  in


  let open UntypedAst in
  match exp with

  (* base cases *)
  | TrueExp ->
    {exp = TrueExp; t = Bool}

  | FalseExp ->
    {exp = FalseExp; t = Bool}

  | FloatExp fnum ->
    {exp = FloatExp fnum; t = Float}

  | IntExp inum ->
    {exp = IntExp inum; t = Int}

  | StringExp str ->
    {exp = StringExp str; t = Array Char}

  (* Arithmetic bin exp *)
  | AddExp (lhs, rhs) ->
    let t_lhs = type_exp env lhs in
    let t_rhs = type_exp env rhs in



  | SubExp (lhs, rhs) -> ()

  | MulExp (lhs, rhs) -> ()

  | DivExp (lhs, rhs) -> ()

  | EqExp (lhs, rhs) -> ()

  | NeExp (lhs, rhs) -> ()

  | LeExp (lhs, rhs) -> ()

  | GeExp (lhs, rhs) -> ()

  | LtExp (lhs, rhs) -> ()

  | GtExp (lhs, rhs) -> ()

  | AndExp (lhs, rhs) -> ()

  | OrExp (lhs, rhs) -> ()

  | UnaryMinusExp e -> ()

  | UnaryNotExp e -> ()

  | NewExp (mt, e) -> ()

  | CastExp (e, t) -> ()

  | LookupExp (var, idx) -> ()

  | VarExp name -> ()

  | CallExp (name, args) -> ()



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
