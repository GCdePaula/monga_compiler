open AstTypes

(* Expressions *)
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
  | VarExp of id
  | CallExp of id * exp_node list


(* Block, Statements and Definitions *)
type def_node =
  | VarDef of monga_variable
  | FuncDef of id * monga_function_type * block_node

and block_node = {
  var_decs : monga_variable list;
  statements : stat_node list
}

and stat_node =
  | IfElseStat of exp_node * block_node * block_node option
  | WhileStat of exp_node * block_node
  | ReturnStat of exp_node option
  | AssignStat of exp_node * exp_node
  | CallStat of id * exp_node list
  | PutStat of exp_node
  | BlockStat of block_node

(* AST *)
type untyped_tree = def_node list

