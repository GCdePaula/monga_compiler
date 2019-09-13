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
    |  list(def) Eof { 0 }

def:
    | var_def { 0 }
    | func_def { 0 }

var_def:
    | Id Colon m_type Semicolon { 0 }

m_type:
    | Int { 0 }
    | Float { 0 }
    | Char { 0 }
    | Bool { 0 }
    | OpenBracket m_type CloseBracket { 0 }

func_def:
    | Id OpenParen; separated_list(Comma, param); CloseParen; option(pair(Colon, m_type)); block { 0 }

param:
    | Id Colon m_type { 0 }

block:
    | OpenBraces list(stat) CloseBraces { 0 }

stat:
    | var_def { 0 }
    | command { 0 }

command:
    | If exp block { 0 }
    | If exp block Else block { 0 }
    | While exp block { 0 }
    | var Assign exp Semicolon { 0 }
    | Return Semicolon { 0 }
    | Return exp Semicolon { 0 }
    | func_call Semicolon { 0 }
    | Put exp Semicolon { 0 }
    | block { 0 }

var:
    | Id { 0 }
    | primary_expression OpenBracket exp CloseBracket { 0 }

primary_expression:
    | IntNumeral { 0 }
    | FloatNumeral { 0 }
    | StringLiteral { 0 }
    | True { 0 }
    | False { 0 }
    | OpenParen exp CloseParen { 0 }
    | var {0}

unary_exp:
    | primary_expression { 0 }
    | Not unary_exp { 0 }
    | Sub unary_exp { 0 }

mul_exp:
    | unary_exp { 0 }
    | mul_exp Mul unary_exp { 0 }
    | mul_exp Div unary_exp { 0 }

add_exp:
    | mul_exp { 0 }
    | add_exp Add mul_exp { 0 }
    | add_exp Sub mul_exp { 0 }

relational_expression:
    | add_exp { 0 }
    | add_exp Eq add_exp { 0 }
    | add_exp Ne add_exp { 0 }
    | add_exp Gt add_exp { 0 }
    | add_exp Lt add_exp { 0 }
    | add_exp Ge add_exp { 0 }
    | add_exp Le add_exp { 0 }

and_exp:
    | relational_expression { 0 }
    | and_exp And relational_expression { 0 }

or_exp:
    | and_exp { 0 }
    | or_exp Or and_exp { 0 }

exp:
    | or_exp  { 0 }
    | exp As m_type { 0 }
    | New m_type OpenBracket exp CloseBracket { 0 }


func_call:
    | Id OpenParen separated_list(Comma, exp) CloseParen { 0 }


