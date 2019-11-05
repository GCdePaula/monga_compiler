
open TypedAst
open Llvm

exception SurprisedPikachu of string

let gen_code typed_tree =

  let llctx = global_context () in
  let llmodule = create_module llctx "mainmod" in

  let int_format_str = define_global "" (const_stringz llctx "%d\n") llmodule in

  let rec gen_exp exp_node llbuilder =
    match exp_node.exp with
    | VarExp name ->
      ()
    | IntExp inum ->
      ()
    | _ -> raise (SurprisedPikachu "Exp case not implemented yet")
  in

  let rec gen_stat stat_node llbuilder =
    match stat_node with
    | AssignStat (var, exp) ->
      ()
    | PutStat exp ->
      ()
    | _ -> raise (SurprisedPikachu "Stat case not implemented yet")
  in

  llmodule
