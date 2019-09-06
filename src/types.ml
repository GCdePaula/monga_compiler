
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


(* Float values loses precision *)
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


let print_tk = function
  | If -> Printf.printf "If"
  | Then -> Printf.printf "Then"
  | Else -> Printf.printf "Else"
  | Return -> Printf.printf "Return"
  | While -> Printf.printf "While"
  | Int -> Printf.printf "Int"
  | Char -> Printf.printf "Char"
  | Float -> Printf.printf "Float"
  | Void -> Printf.printf "Void"
  | StringLiteral str -> Printf.printf "StringLiteral(%s)" str
  | FloatNumeral f -> Printf.printf "FloatNumeral(%h)" f
  | IntNumeral i -> Printf.printf "IntNumeral(%d)" i
  | Put -> Printf.printf "@"
  | Colon -> Printf.printf ":"
  | Semicolon -> Printf.printf ";"
  | Comma -> Printf.printf ","
  | Assign -> Printf.printf "="
  | OpenBracket -> Printf.printf "["
  | CloseBracket -> Printf.printf "]"
  | OpenParen -> Printf.printf "("
  | CloseParen -> Printf.printf ")"
  | OpenBraces -> Printf.printf "{"
  | CloseBraces -> Printf.printf "}"
  | New -> Printf.printf "New"
  | As -> Printf.printf "As"
  | Id str -> Printf.printf "Id(%s)" str
  | Add -> Printf.printf "+"
  | Sub -> Printf.printf "-"
  | Mul -> Printf.printf "*"
  | Div -> Printf.printf "/"
  | Eq -> Printf.printf "=="
  | Ne -> Printf.printf "~="
  | Le -> Printf.printf "<="
  | Ge -> Printf.printf ">="
  | Lt -> Printf.printf "<"
  | Gt -> Printf.printf ">"
  | And -> Printf.printf "&&"
  | Or -> Printf.printf "||"
  | Not -> Printf.printf "!"
  | Eof -> Printf.printf "EOF"


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

