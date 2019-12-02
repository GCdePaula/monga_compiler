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

type t_var_node =
  | SimpleVar of monga_variable
  | LookupVar of t_exp_node * t_exp_node

(* Typed Block and Statements *)
type t_block_node = {
  var_decs : monga_variable list;
  statements : t_stat_node list
}

and t_stat_node =
  | IfElseStat of t_exp_node * t_block_node * t_block_node option
  | WhileStat of t_exp_node * t_block_node
  | ReturnStat of t_exp_node option
  | AssignStat of t_var_node * t_exp_node
  | CallStat of id * t_exp_node list
  | PutStat of t_exp_node
  | BlockStat of t_block_node


(* Typed definitions *)
type t_def_node =
  | VarDef of monga_variable
  | FuncDef of id * monga_function_type * t_block_node


(* Typed AST *)
type typed_tree = t_def_node list


(* Type Errors *)
type type_error =
  | IncompatibleTypeError of monga_type * monga_type (*got, want*)
  | IncompatibleRetType
  | NotArithmeticTypeError of monga_type
  | IndexTypeError of monga_type * monga_type (*var, idx*)
  | WrongNumberOfArgs of int * int
  | UnboundName of id
  | NotAssignable
  | NotAVar of id
  | NotAFunc of id
  | RedeclaredName of id
  | InvalidCast of monga_type * monga_type

type error = {
  loc: location;
  err: type_error
}

