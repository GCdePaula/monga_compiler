{

open Lexing
open Types

exception LexerError of string

let increment_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }

}

(* Patterns *)
let exa_start = '0' ['X' 'x']
let digit = ['0'-'9']
let exa_digit = ['0'-'9' 'a'-'f' 'A'-'F']

let exp = ['e' 'E'] ['-' '+']? digit+
let exa_exp = ['p' 'P'] ['-' '+']? digit+

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' '0'-'9' '_']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Lexer definition *)
rule monga_lexer =
  parse
  | '"' {read_string (Buffer.create 32) lexbuf}

  | digit+
  | exa_start exa_digit+ as inum
    {
      try IntNumeral (int_of_string inum)
      with Failure _ -> raise (LexerError ("Invalid integer literal: " ^ inum))
    }

  | digit* ('.' digit*)? exp?
  | exa_start exa_digit* ('.' exa_digit*)? exa_exp? as fnum
    {
      FloatNumeral (float_of_string fnum)
    }

  | "if" {If}
  | "then" {Then}
  | "else" {Else}
  | "return" {Return}
  | "while" {While}
  | "int" {Int}
  | "char" {Char}
  | "float" {Float}
  | "bool" {Bool}
  | "new" {New}
  | "as" {As}
  | "true" {True}
  | "false" {False}

  | '@' {Put}
  | ':' {Colon}
  | ';' {Semicolon}
  | ',' {Comma}
  | '=' {Assign}
  | '[' {OpenBracket} | ']' {CloseBracket}
  | '(' {OpenParen} | ')' {CloseParen}
  | '{' {OpenBraces} | '}' {CloseBraces}

  | '+' {Add} | '-' {Sub} | '*' {Mul} | '/' {Div}
  | "==" {Eq} | "~=" {Ne} | "<=" {Le} | ">=" {Ge} | '<' {Lt} | '>' {Gt}
  | "&&" {And} | "||" {Or}  | '!' {Not}

  | id as identifier {Id identifier}

  | '#' [^'\n']* '\n' {monga_lexer lexbuf}
  | white* { monga_lexer lexbuf }
  | newline { increment_line lexbuf; monga_lexer lexbuf }

  | _ { raise (LexerError("Unrecognized character: " ^ Lexing.lexeme lexbuf)) }
  | eof { Eof }

and read_string buf =
  parse
  | '"'       { StringLiteral (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\' '\n']+ as str
    {
      Buffer.add_string buf str;
      read_string buf lexbuf
    }
  | '\n' { Buffer.add_char buf '\n'; increment_line lexbuf; read_string buf lexbuf }
  | _ { raise (LexerError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (LexerError ("String is not terminated")) }

{ }

