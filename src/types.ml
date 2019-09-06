
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

open Printf

let string_of_tk = function
  | If -> printf "If"
  | Then -> printf "Then"
  | Else -> printf "Else"
  | Return -> printf "Return"
  | While -> printf "While"
  | Int -> printf "Int"
  | Char -> printf "Char"
  | Float -> printf "Float"
  | Void -> printf "Void"
  | StringLiteral str -> printf "StringLiteral(%s)" str
  | FloatNumeral f -> printf "FloatNumeral(%h)" f
  | IntNumeral i -> printf "IntNumeral(%d)" i
  | Put -> printf "@"
  | Colon -> printf ":"
  | Semicolon -> printf ";"
  | Comma -> printf ","
  | Assign -> printf "="
  | OpenBracket -> printf "["
  | CloseBracket -> printf "]"
  | OpenParen -> printf "("
  | CloseParen -> printf ")"
  | OpenBraces -> printf "{"
  | CloseBraces -> printf "}"
  | New -> printf "New"
  | As -> printf "As"
  | Id str -> printf "Id(%s)" str
  | Add -> printf "+"
  | Sub -> printf "-"
  | Mul -> printf "*"
  | Div -> printf "/"
  | Eq -> printf "=="
  | Ne -> printf "~="
  | Le -> printf "<="
  | Ge -> printf ">="
  | Lt -> printf "<"
  | Gt -> printf ">"
  | And -> printf "&&"
  | Or -> printf "||"
  | Not -> printf "!"
  | Eof -> printf "EOF"


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

