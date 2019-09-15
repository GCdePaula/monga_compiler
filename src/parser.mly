%token IF ELSE RETURN WHILE
%token INT CHAR FLOAT BOOL
%token <string> STRINGLITERAL
%token <float> FLOATNUMERAL
%token <int> INTNUMERAL
%token TRUE FALSE
%token PUT
%token COLON SEMICOLON COMMA
%token ASSIGN
%token OPENBRACKET CLOSEBRACKET
%token OPENPAREN CLOSEPAREN
%token OPENBRACES CLOSEBRACES
%token NEW
%token AS
%token <string> ID
%token ADD SUB MUL DIV
%token EQ NE LE GE LT GT
%token AND OR NOT
%token EOF

%type <Ast_nodes.exp_node> exp var primary_expression unary_exp mul_exp add_exp relational_expression and_exp or_exp
%type <Ast_nodes.stat_node> stat
%type <string * Ast_nodes.exp_node list> func_call
%start <Ast_nodes.def_node list> program

%%

program :
  | def_l = list(def) EOF { def_l }

def:
  | v = var_def { let (id, t) = v in Ast_nodes.VarDef (id, t) }
  | f = func_def { f }

var_def:
  | id = ID COLON t = m_type SEMICOLON {  (id, t) }

m_type:
  | INT { Ast_nodes.Int }
  | FLOAT { Ast_nodes.Float }
  | CHAR { Ast_nodes.Char }
  | BOOL { Ast_nodes.Bool }
  | OPENBRACKET t = m_type CLOSEBRACKET { Ast_nodes.Array t }

func_def:
  | id = ID OPENPAREN p = separated_list(COMMA, param) CLOSEPAREN b = block
    { Ast_nodes.FuncDef (id, p, None, b) }
  | id = ID OPENPAREN p = separated_list(COMMA, param) CLOSEPAREN COLON t = m_type b = block
    { Ast_nodes.FuncDef (id, p, Some t, b) }

param:
  | id = ID COLON t = m_type { (id, t) }

block:
  | OPENBRACES s = list(stat) CLOSEBRACES { Ast_nodes.Stats s }

stat:
  | IF e = exp b = block
    { Ast_nodes.IfElseStat (e, b, None)  }
  | IF e = exp b1 = block ELSE b2 = block
    { Ast_nodes.IfElseStat (e, b1, (Some b2)) }
  | WHILE e = exp b = block
    { Ast_nodes.WhileStat (e, b) }
  | id = var ASSIGN e = exp SEMICOLON
    { Ast_nodes.AssignStat (id, e) }
  | RETURN SEMICOLON
    { Ast_nodes.ReturnStat None }
  | RETURN e = exp SEMICOLON
    { Ast_nodes.ReturnStat (Some e) }
  | f = func_call SEMICOLON
    { let (id, param_list) = f in Ast_nodes.CallStat (id, param_list) }
  | PUT e = exp SEMICOLON
    { Ast_nodes.PutStat e }
  | b = block
    { Ast_nodes.BlockStat b }
  | v = var_def { let (id, t) = v in Ast_nodes.VarDefStat (id, t) }

var:
  | id = ID { Ast_nodes.IdExp id }
  | e1 = primary_expression OPENBRACKET e2 = exp CLOSEBRACKET { Ast_nodes.LookupExp (e1, e2) }

primary_expression:
  | i = INTNUMERAL { Ast_nodes.IntExp i }
  | f = FLOATNUMERAL { Ast_nodes.FloatExp f }
  | s = STRINGLITERAL { Ast_nodes.StringExp s }
  | TRUE { Ast_nodes.TrueExp }
  | FALSE { Ast_nodes.FalseExp }
  | OPENPAREN e = exp CLOSEPAREN { e }
  | f = func_call { let (id, param_list) = f in Ast_nodes.CallExp (id, param_list) }
  | e = var { e }

unary_exp:
  | e = primary_expression { e }
  | NOT e = unary_exp { Ast_nodes.UnaryNotExp e }
  | SUB e = unary_exp { Ast_nodes.UnaryMinusExp e }

mul_exp:
  | e = unary_exp { e }
  | e1 = mul_exp MUL e2 = unary_exp { Ast_nodes.MulExp (e1, e2) }
  | e1 = mul_exp DIV e2 = unary_exp { Ast_nodes.DivExp (e1, e2) }

add_exp:
  | e = mul_exp { e }
  | e1 = add_exp ADD e2 = mul_exp { Ast_nodes.AddExp (e1, e2) }
  | e1 = add_exp SUB e2 = mul_exp { Ast_nodes.SubExp (e1, e2) }

relational_expression:
  | e = add_exp { e }
  | e1 = add_exp EQ e2 = add_exp { Ast_nodes.EqExp (e1, e2) }
  | e1 = add_exp NE e2 = add_exp { Ast_nodes.NeExp (e1, e2) }
  | e1 = add_exp GT e2 = add_exp { Ast_nodes.GtExp (e1, e2) }
  | e1 = add_exp LT e2 = add_exp { Ast_nodes.LtExp (e1, e2) }
  | e1 = add_exp GE e2 = add_exp { Ast_nodes.GeExp (e1, e2) }
  | e1 = add_exp LE e2 = add_exp { Ast_nodes.LeExp (e1, e2) }

and_exp:
  | e = relational_expression { e }
  | e1 = and_exp AND e2 = relational_expression { Ast_nodes.AndExp (e1, e2) }

or_exp:
  | e = and_exp { e }
    | e1 = or_exp OR e2 = and_exp { Ast_nodes.OrExp (e1, e2) }

exp:
  | e = or_exp  { e }
  | e = exp AS t = m_type { Ast_nodes.CastExp (e, t) }
  | NEW t = m_type OPENBRACKET e = exp CLOSEBRACKET { Ast_nodes.NewExp (t, e) }


func_call:
  | id = ID OPENPAREN lexp = separated_list(COMMA, exp) CLOSEPAREN { (id, lexp) }


