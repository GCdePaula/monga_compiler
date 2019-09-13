%token If Else Return While
%token Int Char Float Bool
%token <string> StringLiteral
%token <float> FloatNumeral
%token <int> IntNumeral
%token True False
%token Put
%token Colon Semicolon Comma
%token Assign
%token OpenBracket CloseBracket
%token OpenParen CloseParen
%token OpenBraces CloseBraces
%token New
%token As
%token <string> Id
%token Add Sub Mul Div
%token Eq Ne Le Ge Lt Gt
%token And Or Not
%token Eof

%start <int> program

%%

program :
    |  defs Eof { 0 }

defs:
    | (* empty *) { 0 }
    |  def defs { 0 }

def:
    | var_def {}
    | func_def {}

var_def:
    | Id Colon m_type Semicolon {}

m_type:
    | Int {}
    | Float {}
    | Char {}
    | Bool {}
    | OpenBracket m_type CloseBracket {}

func_def:
    | Id OpenParen params CloseBracket Colon m_type block {}
    | Id OpenParen params CloseBracket Colon block {}

params:
    | (* empty *) {}
    | param params {}

param:
    | Id Colon m_type {}

block:
    | OpenBraces stats CloseBraces {}

stats:
    | (* empty *) {}
    | var_def stats {}
    | command stats {}

command:
    | If exp block {}
    | If exp block Else block {}
    | While exp block {}
    | var Assign exp Semicolon {}
    | Return Semicolon {}
    | Return exp Semicolon {}
    | func_call Semicolon {}
    | Put exp Semicolon {}
    | block {}


primary_expression:
    | IntNumeral {}
    | FloatNumeral {}
    | StringLiteral {}
    | True {}
    | False {}
    | OpenParen exp CloseParen {}
    | var {}

var:
    | Id {}
    | primary_expression OpenBracket exp CloseBracket {}

unary_exp:
    | var {}
    | Not unary_exp {}
    | Sub unary_exp {}

mul_exp:
    | unary_exp {}
    | mul_exp Mul unary_exp {}
    | mul_exp Div unary_exp {}

add_exp:
    | mul_exp {}
    | add_exp Add mul_exp {}
    | add_exp Sub mul_exp {}

relational_expression:
    | add_exp {}
    | add_exp Eq add_exp {}
    | add_exp Ne add_exp {}
    | add_exp Gt add_exp {}
    | add_exp Lt add_exp {}
    | add_exp Ge add_exp {}
    | add_exp Le add_exp {}

and_exp:
    | relational_expression {}
    | and_exp And relational_expression {}

or_exp:
    | and_exp {}
    | or_exp Or and_exp {}

exp:
    | or_exp  {}
    | exp As m_type {}
    | New m_type OpenBracket exp CloseBracket {}


func_call:
    | Id OpenParen exp_list CloseParen {}

exp_list:
    | (* empty *) {}
    | comma_exp exp {}

comma_exp:
    | (* empty *) {}
    | comma_exp exp Comma {}


