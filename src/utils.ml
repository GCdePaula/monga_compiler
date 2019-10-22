open Parser
open AstTypes
open Printf


(* Float numerals might lose precision *)
let string_of_tk = function
  | IF -> "If"
  | ELSE -> "Else"
  | RETURN -> "Return"
  | WHILE -> "While"
  | INT -> "Int"
  | CHAR -> "Char"
  | FLOAT -> "Float"
  | BOOL -> "Bool"
  | STRINGLITERAL str -> "StringLiteral(" ^ str ^ ")"
  | FLOATNUMERAL f -> "FloatNumeral(" ^ (string_of_float f) ^ ")"
  | INTNUMERAL i -> "IntNumeral(" ^ (string_of_int i) ^ ")"
  | TRUE -> "True"
  | FALSE -> "False"
  | PUT -> "@"
  | COLON -> ":"
  | SEMICOLON -> ";"
  | COMMA -> ","
  | ASSIGN -> "="
  | OPENBRACKET -> "["
  | CLOSEBRACKET -> "]"
  | OPENPAREN -> "("
  | CLOSEPAREN -> ")"
  | OPENBRACES -> "{"
  | CLOSEBRACES -> "}"
  | NEW -> "New"
  | AS -> "As"
  | ID str -> "Id(" ^ str ^ ")"
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | EQ -> "=="
  | NE -> "~="
  | LE -> "<="
  | GE -> ">="
  | LT -> "<"
  | GT -> ">"
  | AND -> "&&"
  | OR -> "||"
  | NOT -> "!"
  | EOF -> "EOF"


let print_tk = function
  | IF -> printf "If"
  | ELSE -> printf "Else"
  | RETURN -> printf "Return"
  | WHILE -> printf "While"
  | INT -> printf "Int"
  | CHAR -> printf "Char"
  | FLOAT -> printf "Float"
  | BOOL -> printf "Bool"
  | STRINGLITERAL str -> printf "StringLiteral(%s)" str
  | FLOATNUMERAL f -> printf "FloatNumeral(%h)" f
  | INTNUMERAL i -> printf "IntNumeral(%d)" i
  | TRUE -> printf "True"
  | FALSE -> printf "False"
  | PUT -> printf "@"
  | COLON -> printf ":"
  | SEMICOLON -> printf ";"
  | COMMA -> printf ","
  | ASSIGN -> printf "="
  | OPENBRACKET -> printf "["
  | CLOSEBRACKET -> printf "]"
  | OPENPAREN -> printf "("
  | CLOSEPAREN -> printf ")"
  | OPENBRACES -> printf "{"
  | CLOSEBRACES -> printf "}"
  | NEW -> printf "New"
  | AS -> printf "As"
  | ID str -> printf "Id(%s)" str
  | ADD -> printf "+"
  | SUB -> printf "-"
  | MUL -> printf "*"
  | DIV -> printf "/"
  | EQ -> printf "=="
  | NE -> printf "~="
  | LE -> printf "<="
  | GE -> printf ">="
  | LT -> printf "<"
  | GT -> printf ">"
  | AND -> printf "&&"
  | OR -> printf "||"
  | NOT -> printf "!"
  | EOF -> printf "EOF"



let rec string_of_monga_type t =
  match t with
  | Char -> "char"
  | Int -> "int"
  | Float -> "float"
  | Bool ->  "bool"
  | Array t2 -> "[" ^ (string_of_monga_type t2) ^ "]"


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
  let open UntypedAst in
  let rec print_sig (var_list : monga_variable list) =
    match var_list with
    | [] -> ()
    | [m_var] ->
      printf "%s : " m_var.name;
      print_monga_type m_var.t;
    | m_var :: xs ->
      printf "%s : " m_var.name;
      print_monga_type m_var.t;
      print_string ", ";
      print_sig xs
  in

  match def with
  | VarDef vdef ->
    printf "var_def begin\n\tid = %s\n\ttype = " vdef.name;
    print_monga_type vdef.t;
    print_string "\nvar_def end\n\n"

  | FuncDef (name, signature, b) ->
    printf "func_def begin\n\tid = %s\n\tsignature = (" name;
    print_sig signature.parameters;
    print_string ")";
    (match signature.ret_type with
      | Some t ->
        print_string "\n\treturn_type = ";
        print_monga_type t
      | None -> ()
    );
    print_string "\n\tblock = ";
    print_block b 1;
    print_string "func_def end\n\n"

and print_block block depth =
  let p_var (m_var : monga_variable) =
    print_depth (depth+1); print_string "var_def begin\n";
    print_depth (depth+2); printf "id = %s\n\n" m_var.name;
    print_depth (depth+2); print_string "type = ";
    print_monga_type m_var.t; print_string "\n";
    print_string "var_def end\n\n"
  in
  let p_stat stat = print_stat stat (depth+1) in
  print_string "block begin\n";

  List.iter p_var block.var_decs;
  List.iter p_stat block.statements;

  print_depth depth; print_string "block end\n"

and print_stat stat depth =
  match stat with
  | IfElseStat (exp, then_block, else_block_opt) ->
    print_depth depth; print_string "if_else_statement begin\n";
    print_depth (depth+1); print_string "condition = ";
    print_exp exp (depth+1); print_string "\n";
    print_depth (depth+1); print_string "then_block = ";
    print_block then_block (depth+1);
    (match else_block_opt with
    | Some else_block ->
      print_string "\n";
      print_depth (depth+1); print_string "else_block = ";
      print_block else_block (depth+1);
    | None -> ());
    print_depth depth; print_string "if_else_statement end\n\n"

  | WhileStat (exp, block) ->
    print_depth depth; print_string "while_statement begin\n";
    print_depth (depth+1); print_string "condition = ";
    print_exp exp (depth+1); print_string "\n";
    print_depth (depth+1); print_string "block = ";
    print_block block (depth+1);
    print_depth depth; print_string "while_statement end\n\n"

  | ReturnStat (opt_exp) ->
    print_depth depth; print_string "return_statement begin\n";
    (match opt_exp with
     | Some exp ->
       print_depth (depth+1); print_string "return_exp = ";
       print_exp exp (depth+1);
     | None -> ());
    print_depth depth; print_string "return_statement end\n\n"

  | AssignStat (exp1, exp2) ->
    print_depth depth; print_string "assign_stat begin\n";
    print_depth (depth+1); print_string "lhs = ";
    print_exp exp1 (depth+1); print_string "\n";
    print_depth (depth+1); print_string "rhs = ";
    print_exp exp2 (depth+1);
    print_depth depth; print_string "assign_stat end\n\n"

  | CallStat (fname, lexp) ->
    print_depth depth; print_string "fcall_stat begin\n";
    print_depth (depth+1); printf "name = %s\n" fname;
    print_depth (depth+1); print_string "parameters = ";
    print_exp_list lexp (depth+2);
    print_depth depth; print_string "fcall_stat end\n\n"

  | PutStat (exp) ->
    print_depth depth; print_string "put_stat begin\n";
    print_depth (depth+1); print_string "exp = ";
    print_exp exp (depth+1);
    print_depth depth; print_string "put_stat end\n\n"

  | BlockStat (block) ->
    print_depth (depth); print_block block (depth);

and print_exp exp depth =
  let print_bin_exp lhs rhs =
    print_depth (depth+1); print_string "lhs = ";
    print_exp lhs (depth+1); print_string "\n";
    print_depth (depth+1); print_string "rhs = ";
    print_exp rhs (depth+1)
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
    print_monga_type t; print_string "\n\n";
    print_depth (depth+1); print_string "size = ";
    print_exp exp (depth+2);
    print_depth depth; print_string "new_exp end\n"

  | CastExp (exp, t) ->
    print_string "cast_exp begin\n";
    print_depth (depth+1); print_string "exp = ";
    print_exp exp (depth+2); print_string "\n";
    print_depth (depth+1); print_string "type = ";
    print_monga_type t; print_string "\n";
    print_depth depth; print_string "lookup_exp end\n"

  | LookupExp (exp1, exp2) ->
    print_string "lookup_exp begin\n";
    print_depth (depth+1); print_string "var = ";
    print_exp exp1 (depth+1); print_string "\n";
    print_depth (depth+1); print_string "idx = ";
    print_exp exp2 (depth+1);
    print_depth depth; print_string "lookup_exp end\n"

  | VarExp id ->
    printf "%s\n" id

  | CallExp (fname, params) ->
    print_string "call_exp begin\n";
    print_depth (depth+1); printf "name = %s\n\n" fname;
    print_depth (depth+1); print_string "parameters = ";
    print_exp_list params (depth+2);
    print_depth depth; print_string "call_exp end\n"

and print_exp_list exp_list depth =
  let p_single_exp exp =
    print_depth (depth);
    print_exp exp (depth)
  in

  let rec p_lexp lexp =
    match lexp with
    | [] -> ()
    | [exp] ->
      p_single_exp exp
    | exp :: exps ->
      p_single_exp exp;
      print_depth depth; print_string ";\n";
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




let rec print_typed_def (def : TypedAst.t_def_node) =
  let open TypedAst in
  let rec print_sig (var_list : monga_variable list) =
    match var_list with
    | [] -> ()
    | [m_var] ->
      printf "%s : " m_var.name;
      print_monga_type m_var.t;
    | m_var :: xs ->
      printf "%s : " m_var.name;
      print_monga_type m_var.t;
      print_string ", ";
      print_sig xs
  in

  match def with
  | VarDef vdef ->
    printf "var_def begin\n\tid = %s\n\ttype = " vdef.name;
    print_monga_type vdef.t;
    print_string "\nvar_def end\n\n"

  | FuncDef (name, signature, b) ->
    printf "func_def begin\n\tid = %s\n\tsignature = (" name;
    print_sig signature.parameters;
    print_string ")";
    (match signature.ret_type with
      | Some t ->
        print_string "\n\treturn_type = ";
        print_monga_type t
      | None -> ()
    );
    print_string "\n\tblock = ";
    print_typed_block b 1;
    print_string "func_def end\n\n"

and print_typed_block (block : TypedAst.t_block_node) depth =
  let p_var (m_var : monga_variable) =
    print_depth (depth+1); print_string "var_def begin\n";
    print_depth (depth+2); printf "id = %s\n\n" m_var.name;
    print_depth (depth+2); print_string "type = ";
    print_monga_type m_var.t; print_string "\n";
    print_string "var_def end\n\n"
  in
  let p_stat stat = print_typed_stat stat (depth+1) in
  print_string "block begin\n";

  List.iter p_var block.var_decs;
  List.iter p_stat block.statements;

  print_depth depth; print_string "block end\n"

and print_typed_stat (stat : TypedAst.t_stat_node) depth =
  match stat with
  | IfElseStat (exp, then_block, else_block_opt) ->
    print_depth depth; print_string "if_else_statement begin\n";
    print_depth (depth+1); print_string "condition = ";
    print_typed_exp exp (depth+1); print_string "\n";
    print_depth (depth+1); print_string "then_block = ";
    print_typed_block then_block (depth+1);
    (match else_block_opt with
    | Some else_block ->
      print_string "\n";
      print_depth (depth+1); print_string "else_block = ";
      print_typed_block else_block (depth+1);
    | None -> ());
    print_depth depth; print_string "if_else_statement end\n\n"

  | WhileStat (exp, block) ->
    print_depth depth; print_string "while_statement begin\n";
    print_depth (depth+1); print_string "condition = ";
    print_typed_exp exp (depth+1); print_string "\n";
    print_depth (depth+1); print_string "block = ";
    print_typed_block block (depth+1);
    print_depth depth; print_string "while_statement end\n\n"

  | ReturnStat (opt_exp) ->
    print_depth depth; print_string "return_statement begin\n";
    (match opt_exp with
     | Some exp ->
       print_depth (depth+1); print_string "return_exp = ";
       print_typed_exp exp (depth+1);
     | None -> ());
    print_depth depth; print_string "return_statement end\n\n"

  | AssignStat (exp1, exp2) ->
    print_depth depth; print_string "assign_stat begin\n";
    print_depth (depth+1); print_string "lhs = ";
    print_typed_exp exp1 (depth+1); print_string "\n";
    print_depth (depth+1); print_string "rhs = ";
    print_typed_exp exp2 (depth+1);
    print_depth depth; print_string "assign_stat end\n\n"

  | CallStat (fname, lexp) ->
    print_depth depth; print_string "fcall_stat begin\n";
    print_depth (depth+1); printf "name = %s\n" fname;
    print_depth (depth+1); print_string "parameters = ";
    print_typed_exp_list lexp (depth+2);
    print_depth depth; print_string "fcall_stat end\n\n"

  | PutStat (exp) ->
    print_depth depth; print_string "put_stat begin\n";
    print_depth (depth+1); print_string "exp = ";
    print_typed_exp exp (depth+1);
    print_depth depth; print_string "put_stat end\n\n"

  | BlockStat (block) ->
    print_depth (depth); print_typed_block block (depth);

and print_typed_exp (exp : TypedAst.t_exp_node) depth =
  let print_bin_exp lhs rhs =
    print_depth (depth+1); print_string "lhs = ";
    print_typed_exp lhs (depth+1); print_string "\n";
    print_depth (depth+1); print_string "rhs = ";
    print_typed_exp rhs (depth+1)
  in

  let print_type e =
    print_string "{"; print_monga_type e; print_string "}\n"
  in

  match exp.exp with
  | AddExp (exp1, exp2) ->
    print_string "add_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "add_exp end\n"

  | SubExp (exp1, exp2) ->
    print_string "sub_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "sub_exp end\n"

  | MulExp (exp1, exp2) ->
    print_string "mul_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "mul_exp end\n"

  | DivExp (exp1, exp2) ->
    print_string "div_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "div_exp end\n"

  | EqExp (exp1, exp2) ->
    print_string "equals_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "equals_exp end\n"

  | NeExp (exp1, exp2) ->
    print_string "add_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "add_exp end\n"

  | LeExp (exp1, exp2) ->
    print_string "less_equal_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "less_equal_exp end\n"

  | GeExp (exp1, exp2) ->
    print_string "greater_equal_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "greater_equal_exp end\n"

  | LtExp (exp1, exp2) ->
    print_string "less_than_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "less_than_exp end\n"

  | GtExp (exp1, exp2) ->
    print_string "greater_than_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "greater_than_exp end\n"

  | AndExp (exp1, exp2) ->
    print_string "and_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "and_exp end\n"

  | OrExp (exp1, exp2) ->
    print_string "or_exp begin"; print_type exp.t;
    print_bin_exp exp1 exp2;
    print_depth depth; print_string "or_exp end\n"

  | UnaryMinusExp exp ->
    print_string "unary_minus_exp begin"; print_type exp.t;
    print_depth (depth+1); print_string "exp = "; print_typed_exp exp (depth+1);
    print_depth depth; print_string "unary_minus_exp end\n"

  | UnaryNotExp exp ->
    print_string "unary_not_exp begin"; print_type exp.t;
    print_depth (depth+1); print_string "exp = "; print_typed_exp exp (depth+1);
    print_depth depth; print_string "unary_not_exp end\n"

  | TrueExp ->
    print_string "true"; print_type exp.t;

  | FalseExp ->
    print_string "false"; print_type exp.t;

  | FloatExp fnum ->
    printf "%h" fnum; print_type exp.t;

  | IntExp inum ->
    printf "%d" inum; print_type exp.t;

  | StringExp str ->
    printf "\"%s\"" str; print_type exp.t;

  | NewExp (t, exp) ->
    print_string "new_exp begin"; print_type exp.t;
    print_depth (depth+1); print_string "type = ";
    print_monga_type t; print_string "\n\n";
    print_depth (depth+1); print_string "size = ";
    print_typed_exp exp (depth+2);
    print_depth depth; print_string "new_exp end\n"

  | CastExp (exp, t) ->
    print_string "cast_exp begin"; print_type exp.t;
    print_depth (depth+1); print_string "exp = ";
    print_typed_exp exp (depth+2); print_string "\n";
    print_depth (depth+1); print_string "type = ";
    print_monga_type t; print_string "\n";
    print_depth depth; print_string "lookup_exp end\n"

  | LookupExp (exp1, exp2) ->
    print_string "lookup_exp begin"; print_type exp.t;
    print_depth (depth+1); print_string "var = ";
    print_typed_exp exp1 (depth+1); print_string "\n";
    print_depth (depth+1); print_string "idx = ";
    print_typed_exp exp2 (depth+1);
    print_depth depth; print_string "lookup_exp end\n"

  | VarExp id ->
    printf "%s" id; print_type exp.t;

  | CallExp (fname, params) ->
    print_string "call_exp begin"; print_type exp.t;
    print_depth (depth+1); printf "name = %s\n\n" fname;
    print_depth (depth+1); print_string "parameters = ";
    print_typed_exp_list params (depth+2);
    print_depth depth; print_string "call_exp end\n"

and print_typed_exp_list exp_list depth =
  let p_single_exp exp =
    print_depth (depth);
    print_typed_exp exp (depth)
  in

  let rec p_lexp lexp =
    match lexp with
    | [] -> ()
    | [exp] ->
      p_single_exp exp
    | exp :: exps ->
      p_single_exp exp;
      print_depth depth; print_string ";\n";
      p_lexp exps
  in
  print_string "exp_list begin\n";
  p_lexp exp_list;
  print_depth depth; print_string "exp_list end\n"


let rec print_typed_program defs =
  match defs with
  | [] -> ()
  | def :: xs ->
    print_typed_def def;
    print_typed_program xs



let string_of_type_error (err : TypedAst.type_error) =
  match err with
  | IncompatibleTypeError (got, want) ->
    "Got type {" ^
    (string_of_monga_type got) ^
    "}, which is incompatible with expected type {" ^
    (string_of_monga_type want) ^
    "}"

  | IncompatibleRetType ->
    "Incompatible return type"

  | NotArithmeticTypeError t ->
    "Type {" ^
    (string_of_monga_type t) ^
    "} is not an arithmetic type"

  | IndexTypeError (t1, t2) ->
    (match t1, t2 with
    | Array _, _ ->
      "Type {" ^ (string_of_monga_type t2) ^ "} is not an integer"
    | _, Int ->
      "Type {" ^ (string_of_monga_type t1) ^ "} is not an array"
    | _, _ ->
      "Type {" ^ (string_of_monga_type t1) ^ "} is not an array" ^
      ", type {" ^ (string_of_monga_type t2) ^ "} is not an integer")

  | WrongNumberOfArgs (got, want) ->
    "Got " ^ (string_of_int got) ^ "arguments, but expected " ^ (string_of_int want)

  | UnboundName name ->
    "Name not bound " ^ name

  | NotAssignable ->
    "Cannot assign expression"

  | NotAVar name ->
    "Name '" ^ name ^ "' is not a variable"

  | NotAFunc name ->
    "Name '" ^ name ^ "' is not a function"

  | RedeclaredName name ->
    "Name '" ^ name ^ "' redeclaration"

let print_type_error (err : TypedAst.type_error) =
  print_endline (string_of_type_error err)

let print_type_error_list (errs : TypedAst.type_error list) =
  List.iter print_type_error errs

