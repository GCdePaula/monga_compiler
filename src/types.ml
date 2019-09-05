
type token =
  | If
  | Then
  | Else
  | Return
  | While
  | Int | Char | Float | Void
  | StringLiteral of string
  | FloatNumeral of float
  | IntNumeral of int
  | Put
  | Colon
  | Semicolon
  | Comma
  | Assign
  | OpenBracket | CloseBracket
  | OpenParen | CloseParen
  | OpenBraces | CloseBraces
  | New
  | As
  | Id of string
  | Add | Sub | Mul | Div
  | Eq | Ne | Le | Ge | Lt | Gt
  | And | Or | Not
  | Eof



let string_of_tk = function
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | Return -> "Return"
  | While -> "While"
  | Int -> "Int"
  | Char -> "Char"
  | Float -> "Float"
  | Void -> "Void"
  | StringLiteral str -> "StringLiteral(" ^ str ^ ")"
  | FloatNumeral f -> "FloatNumeral(" ^ (string_of_float f) ^ ")"
  | IntNumeral i -> "IntNumeral(" ^ (string_of_int i) ^ ")"
  | Put -> "@"
  | Colon -> ":"
  | Semicolon -> ";"
  | Comma -> ","
  | Assign -> "="
  | OpenBracket -> "["
  | CloseBracket -> "]"
  | OpenParen -> "("
  | CloseParen -> ")"
  | OpenBraces -> "{"
  | CloseBraces -> "}"
  | New -> "New"
  | As -> "As"
  | Id str -> "Id(" ^ str ^ ")"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Ne -> "~="
  | Le -> "<="
  | Ge -> ">="
  | Lt -> "<"
  | Gt -> ">"
  | And -> "&&"
  | Or -> "||"
  | Not -> "!"
  | Eof -> "EOF"


(*type monga_type =*)
  (*| Int*)
  (*| Char*)
  (*| Float*)
  (*| Void*)
  (*| Array of monga_type*)

(*type op =*)
  (*| Add*)
  (*| Sub*)
  (*| Mul*)
  (*| Div*)
  (*| Eq*)
  (*| Ne*)
  (*| Le*)
  (*| Ge*)
  (*| Lt*)
  (*| Gt*)
  (*| And*)
  (*| Or*)
  (*| Not*)


(* conversion functions for printing *)

(*let rec string_of_mt = function*)
  (*| Int -> "Int"*)
  (*| Char -> "Char"*)
  (*| Float -> "Float"*)
  (*| Void -> "Void"*)
  (*| Array tp -> "Array of " ^ (string_of_mt tp)*)

(*let string_of_op = function*)
(*  | Add -> "+"*)
  (*| Sub -> "-"*)
  (*| Mul -> "*"*)
  (*| Div -> "/"*)
  (*| Eq -> "=="*)
  (*| Ne -> "~="*)
  (*| Le -> "<="*)
  (*| Ge -> ">="*)
  (*| Lt -> "<"*)
  (*| Gt -> ">"*)
  (*| And -> "&&"*)
  (*| Or -> "||"*)
  (*| Not -> "!"*)

