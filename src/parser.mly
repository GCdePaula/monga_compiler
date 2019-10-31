%{
  open UntypedAst
%}

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
%token <AstTypes.id> ID
%token ADD SUB MUL DIV
%token EQ NE LE GE LT GT
%token AND OR NOT
%token EOF

%type <UntypedAst.exp_node> exp var primary_expression unary_exp mul_exp add_exp relational_expression and_exp or_exp
%type <UntypedAst.stat_node> stat
%type <AstTypes.id * UntypedAst.exp_node list> func_call
%type <UntypedAst.block_node> block
%type <AstTypes.monga_type> m_type
%type <AstTypes.monga_variable> var_def

%start <UntypedAst.untyped_tree> program

%%

program :
  | def_l = list(def) EOF { def_l }

def:
  | v = var_def SEMICOLON { UntypedAst.VarDef ($loc, v) }
  | f = func_def { f }

var_def:
  | id = ID COLON t = m_type { AstTypes.{name = id; t = t} }

m_type:
  | INT { AstTypes.Int }
  | FLOAT { AstTypes.Float }
  | CHAR { AstTypes.Char }
  | BOOL { AstTypes.Bool }
  | OPENBRACKET t = m_type CLOSEBRACKET { AstTypes.Array t }

func_def:
  | id = ID OPENPAREN p = separated_list(COMMA, var_def) CLOSEPAREN b = block
    { UntypedAst.FuncDef ($loc, id, {parameters = p; ret_type = None}, b) }
  | id = ID OPENPAREN p = separated_list(COMMA, var_def) CLOSEPAREN COLON t = m_type b = block
    { UntypedAst.FuncDef ($loc, id, {parameters = p; ret_type = Some t}, b) }

block:
  | OPENBRACES b = block_content CLOSEBRACES { b }

block_content:
  | v = var_def SEMICOLON vs = block_content
    { UntypedAst.{vs with var_decs = ($loc, v) :: vs.var_decs} }
  | s = list(stat)
    { UntypedAst.{var_decs=[]; statements=s} }

stat:
  | IF e = exp b = block
    { {loc=$loc; stat=UntypedAst.IfElseStat (e, b, None)}  }
  | IF e = exp b1 = block ELSE b2 = block
    { {loc=$loc; stat=UntypedAst.IfElseStat (e, b1, (Some b2))} }
  | WHILE e = exp b = block
    { {loc=$loc; stat=UntypedAst.WhileStat (e, b)} }
  | id = var ASSIGN e = exp SEMICOLON
    { {loc=$loc; stat=UntypedAst.AssignStat (id, e)} }
  | RETURN e = option(exp) SEMICOLON
    { {loc=$loc; stat=UntypedAst.ReturnStat e} }
  | f = func_call SEMICOLON
    { let (id, args_list) = f in {loc=$loc; stat=UntypedAst.CallStat (id, args_list)} }
  | PUT e = exp SEMICOLON
    { {loc=$loc; stat=UntypedAst.PutStat e} }
  | b = block
    { {loc=$loc; stat=UntypedAst.BlockStat b} }

var:
  | id = ID { {loc = $loc; exp = VarExp id} }
  | arr = primary_expression OPENBRACKET idx = exp CLOSEBRACKET
    { {loc = $loc; exp = LookupExp (arr, idx)} }

primary_expression:
  | i = INTNUMERAL { {loc = $loc; exp = IntExp i} }
  | f = FLOATNUMERAL { {loc = $loc; exp = FloatExp f} }
  | s = STRINGLITERAL { {loc = $loc; exp = StringExp s} }
  | TRUE { {loc = $loc; exp = TrueExp} }
  | FALSE { {loc = $loc; exp = FalseExp} }
  | OPENPAREN e = exp CLOSEPAREN { e }
  | f = func_call
    { let (id, args_list) = f in {loc = $loc; exp = CallExp (id, args_list)} }
  | e = var { e }

unary_exp:
  | e = primary_expression { e }
  | NOT e = unary_exp { {loc = $loc; exp = UnaryNotExp e} }
  | SUB e = unary_exp { {loc = $loc; exp = UnaryMinusExp e} }

mul_exp:
  | e = unary_exp { e }
  | e1 = mul_exp MUL e2 = unary_exp { {loc = $loc; exp = MulExp (e1, e2)} }
  | e1 = mul_exp DIV e2 = unary_exp { {loc = $loc; exp = DivExp (e1, e2)} }

add_exp:
  | e = mul_exp { e }
  | e1 = add_exp ADD e2 = mul_exp { {loc = $loc; exp = AddExp (e1, e2)} }
  | e1 = add_exp SUB e2 = mul_exp { {loc = $loc; exp = SubExp (e1, e2)} }

relational_expression:
  | e = add_exp { e }
  | e1 = add_exp EQ e2 = add_exp { {loc = $loc; exp = EqExp (e1, e2)} }
  | e1 = add_exp NE e2 = add_exp { {loc = $loc; exp = NeExp (e1, e2)} }
  | e1 = add_exp GT e2 = add_exp { {loc = $loc; exp = GtExp (e1, e2)} }
  | e1 = add_exp LT e2 = add_exp { {loc = $loc; exp = LtExp (e1, e2)} }
  | e1 = add_exp GE e2 = add_exp { {loc = $loc; exp = GeExp (e1, e2)} }
  | e1 = add_exp LE e2 = add_exp { {loc = $loc; exp = LeExp (e1, e2)} }

and_exp:
  | e = relational_expression { e }
  | e1 = and_exp AND e2 = relational_expression { {loc = $loc; exp = AndExp (e1, e2)} }

or_exp:
  | e = and_exp { e }
  | e1 = or_exp OR e2 = and_exp { {loc = $loc; exp = OrExp (e1, e2)} }

exp:
  | e = or_exp  { e }
  | e = exp AS t = m_type { {loc = $loc; exp = CastExp (e, t)} }
  | NEW t = m_type OPENBRACKET e = exp CLOSEBRACKET { {loc = $loc; exp = NewExp (t, e)} }


func_call:
  | id = ID OPENPAREN args = separated_list(COMMA, exp) CLOSEPAREN { (id, args) }


