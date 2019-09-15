
type monga_type =
  | Int
  | Float
  | Bool
  | Char
  | Array of monga_type

type exp_node =
  | AddExp of exp_node * exp_node
  | SubExp of exp_node * exp_node
  | MulExp of exp_node * exp_node
  | DivExp of exp_node * exp_node
  | EqExp of exp_node * exp_node
  | NeExp of exp_node * exp_node
  | LeExp of exp_node * exp_node
  | GeExp of exp_node * exp_node
  | LtExp of exp_node * exp_node
  | GtExp of exp_node * exp_node
  | AndExp of exp_node * exp_node
  | OrExp of exp_node * exp_node
  | UnaryMinusExp of exp_node
  | UnaryNotExp of exp_node
  | TrueExp | FalseExp
  | FloatExp of float
  | IntExp of int
  | StringExp of string
  | NewExp of monga_type * exp_node
  | CastExp of exp_node * monga_type
  | LookupExp of exp_node * exp_node
  | IdExp of string
  | CallExp of string * exp_node list

and stat_node =
  | IfElseStat of exp_node * block_node * block_node option
  | WhileStat of exp_node * block_node
  | ReturnStat of exp_node option
  | AssignStat of exp_node * exp_node
  | CallStat of string * exp_node list
  | PutStat of exp_node
  | BlockStat of block_node
  | VarDefStat of string * monga_type

and block_node =
  | Stats of stat_node list

and def_node =
  | VarDef of string * monga_type
  | FuncDef of string * (string * monga_type) list * monga_type option * block_node



open Printf

let rec print_depth depth =
  match depth with
  | 0 -> ()
  | n -> print_string "\t"; print_depth (n-1)

let rec print_monga_type t =
  match t with
  | Char -> print_string "char"
  | Int -> print_string "int"
  | Float -> print_string "float"
  | Bool -> print_string "bool"
  | Array t2 -> print_string "["; print_monga_type t2; print_string "]"



let rec print_def def =
  let rec print_sig = function
    | [] -> ()
    | [(id, t)] ->
      printf "%s : " id;
      print_monga_type t;
    | (id, t) :: xs ->
      printf "%s : " id;
      print_monga_type t;
      print_string ", ";
      print_sig xs
  in

  match def with
  | VarDef (id, t) ->
    printf "var_def begin\n\tid = %s\n\ttype = " id;
    print_monga_type t;
    print_string "\nvar_def end\n"

  | FuncDef (id, signature, opt_t, b) ->
    printf "func_def begin\n\tid = %s\n\tsignature = (" id;
    print_sig signature;
    print_string ")";
    (match opt_t with
      | Some t -> 
        print_string "\n\treturn_type = ";
        print_monga_type t
      | None -> ()
    );
    print_string "\n\tblock = ";
    print_block b 1

and print_block block depth =
  let p_stat stat = print_stat stat (depth+1) in
  print_string "block begin\n";

  match block with
  | Stats (lstat) ->
    List.iter p_stat lstat;

  print_depth depth; print_string "block end\n"

and print_stat stat depth =
  match stat with
  | IfElseStat (exp, then_block, else_block_opt) ->
    print_depth depth; print_string "if_else_statement begin\n";
    print_depth (depth+1); print_string "condition = ";
    print_exp exp (depth+1);
    print_depth (depth+1); print_string "then_block = ";
    print_block then_block (depth+1);
    (match else_block_opt with
    | Some else_block ->
      print_depth (depth+1); print_string "else_block = ";
      print_block else_block (depth+1);
    | None -> ());
    print_depth depth; print_string "if_else_statement end\n"

  | WhileStat (exp, block) ->
    print_depth depth; print_string "while_statement begin\n";
   print_depth (depth+1); print_string "condition = ";
    print_exp exp (depth+1);
    print_depth (depth+1); print_string "block = ";
    print_block block (depth+1);
    print_depth depth; print_string "while_statement end\n"

  | ReturnStat (opt_exp) ->
    print_depth depth; print_string "return_statement begin\n";
    (match opt_exp with
     | Some exp ->
       print_depth (depth+1); print_string "return_exp = ";
       print_exp exp (depth+1);
     | None -> ());
    print_depth depth; print_string "return_statement end\n"

  | AssignStat (exp1, exp2) ->
    print_depth depth; print_string "assign_stat begin\n";
    print_depth (depth+1); print_string "lhs = ";
    print_exp exp1 (depth+1);
    print_depth (depth+1); print_string "rhs = ";
    print_exp exp2 (depth+1);
    print_depth depth; print_string "assign_stat end\n"

  | CallStat (fname, lexp) ->
    print_depth depth; print_string "fcall_stat begin\n";
    print_depth (depth+1); printf "name = %s" fname;
    print_depth (depth+1); print_string "parameters = ";
    print_exp_list lexp (depth+2)

  | PutStat (exp) ->
    print_depth depth; print_string "put_stat begin\n";
    print_depth (depth+1); print_string "exp = ";
    print_exp exp (depth+1);
    print_depth depth; print_string "put_stat end\n"

  | BlockStat (block) ->
    print_depth (depth); print_block block (depth)

  | VarDefStat (id, t) ->
    print_depth depth; print_string "var_def begin\n";
    print_depth (depth+1); printf "id = %s\n"id;
    print_depth (depth+1); print_string "type = ";
    print_monga_type t; print_string "\n";
    print_string "var_def end\n"

and print_exp exp depth =
  let print_bin_exp lhs rhs =
    print_depth (depth+1); print_string "lhs = ";
    print_exp lhs (depth+2);
    print_depth (depth+1); print_string "rhs = ";
    print_exp rhs (depth+2)
  in

  match exp with
  | AddExp (exp1, exp2) ->
    print_string "add_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "add_exp end\n"

  | SubExp (exp1, exp2) ->
    print_string "sub_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "sub_exp end\n"

  | MulExp (exp1, exp2) ->
    print_string "mul_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "mul_exp end\n"

  | DivExp (exp1, exp2) ->
    print_string "div_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "div_exp end\n"

  | EqExp (exp1, exp2) ->
    print_string "equals_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "equals_exp end\n"

  | NeExp (exp1, exp2) ->
    print_string "add_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "add_exp end\n"

  | LeExp (exp1, exp2) ->
    print_string "less_equal_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "less_equal_exp end\n"

  | GeExp (exp1, exp2) ->
    print_string "greater_equal_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "greater_equal_exp end\n"

  | LtExp (exp1, exp2) ->
    print_string "less_than_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "less_than_exp end\n"

  | GtExp (exp1, exp2) ->
    print_string "greater_than_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "greater_than_exp end\n"

  | AndExp (exp1, exp2) ->
    print_string "and_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "and_exp end\n"

  | OrExp (exp1, exp2) ->
    print_string "or_exp begin\n";
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "or_exp end\n"

  | UnaryMinusExp exp ->
    print_string "unary_minus_exp begin\n";
    print_depth (depth+1); print_string "exp = "; print_exp exp (depth+1);
    print_depth depth; print_string "unary_minus_exp end\n"

  | UnaryNotExp exp ->
    print_string "unary_not_exp begin\n";
    print_depth (depth+1); print_string "exp = "; print_exp exp (depth+1);
    print_depth depth; print_string "unary_not_exp end\n"

  | TrueExp ->
    print_string "true\n"

  | FalseExp ->
    print_string "false\n"

  | FloatExp fnum ->
    printf "%h\n" fnum

  | IntExp inum ->
    printf "%d\n" inum

  | StringExp str ->
    printf "\"%s\"\n" str

  | NewExp (t, exp) ->
    print_string "new_exp begin\n";
    print_depth (depth+1); print_string "type = ";
    print_monga_type t; print_string "\n";
    print_depth (depth+1); print_string "size = ";
    print_exp exp (depth+2);
    print_depth depth; print_string "new_exp end\n"

  | CastExp (exp, t) ->
    print_string "cast_exp begin\n";
    print_depth (depth+1); print_string "exp = ";
    print_exp exp (depth+2);
    print_depth (depth+1); print_string "type = ";
    print_monga_type t; print_string "\n";
    print_depth depth; print_string "lookup_exp end\n"

  | LookupExp (exp1, exp2) ->
    print_string "lookup_exp begin\n";
    print_depth (depth+1); print_string "var = ";
    print_exp exp1 (depth+2);
    print_depth (depth+1); print_string "idx = ";
    print_exp exp2 (depth+2);
    print_depth depth; print_string "lookup_exp end\n"

  | IdExp id ->
    printf "%s\n" id

  | CallExp (fname, params) ->
    print_string "call_exp begin\n";
    print_depth (depth+1); printf "name = %s" fname;
    print_depth (depth+1); print_string "parameters = ";
    print_exp_list params (depth+2);
    print_depth depth; print_string "call_exp end\n"

and print_exp_list exp_list depth =
  let p_single_exp exp =
    print_depth (depth+1);
    print_exp exp (depth+1)
  in

  let rec p_lexp lexp =
    match lexp with
    | [] -> ()
    | [exp] ->
      p_single_exp exp
    | exp :: exps ->
      p_single_exp exp; print_string ",\n";
      p_lexp exps
  in
  print_string "exp_list begin\n";
  p_lexp exp_list;
  print_depth depth; print_string "exp_list end\n"


let rec print_program defs =
  match defs with
  | [] -> ()
  | def :: xs ->
    print_def def;
    print_program xs

