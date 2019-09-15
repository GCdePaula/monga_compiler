{

open Lexing
(*open Types*)
open Parser

exception LexerError of string

let incr_line lexbuf =
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
  | '"' {read_string (Buffer.create 32) lexbuf.lex_start_p lexbuf}

  | digit+
  | exa_start exa_digit+ as inum
    {
      try INTNUMERAL (int_of_string inum)
      with Failure _ -> raise (LexerError ("Invalid integer literal: " ^ inum))
    }

  | digit* ('.' digit*)? exp?
  | exa_start exa_digit* ('.' exa_digit*)? exa_exp? as fnum
    {
      FLOATNUMERAL (float_of_string fnum)
    }

  | "if" {IF}
  | "else" {ELSE}
  | "return" {RETURN}
  | "while" {WHILE}
  | "int" {INT}
  | "char" {CHAR}
  | "float" {FLOAT}
  | "bool" {BOOL}
  | "new" {NEW}
  | "as" {AS}
  | "true" {TRUE}
  | "false" {FALSE}

  | '@' {PUT}
  | ':' {COLON}
  | ';' {SEMICOLON}
  | ',' {COMMA}
  | '=' {ASSIGN}
  | '[' {OPENBRACKET} | ']' {CLOSEBRACKET}
  | '(' {OPENPAREN} | ')' {CLOSEPAREN}
  | '{' {OPENBRACES} | '}' {CLOSEBRACES}

  | '+' {ADD} | '-' {SUB} | '*' {MUL} | '/' {DIV}
  | "==" {EQ} | "~=" {NE} | "<=" {LE} | ">=" {GE} | '<' {LT} | '>' {GT}
  | "&&" {AND} | "||" {OR}  | '!' {NOT}

  | id as identifier {ID identifier}

            | '#' [^'\n']* '\n' {incr_line lexbuf; monga_lexer lexbuf}
  | white* { monga_lexer lexbuf }
  | newline { incr_line lexbuf; monga_lexer lexbuf }

  | _ { raise (LexerError("Unrecognized character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_string buf start_p =
  parse
  | '"'       { lexbuf.lex_start_p <- start_p; STRINGLITERAL (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf start_p lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf start_p lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf start_p lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf start_p lexbuf }
  | [^ '"' '\\' '\n']+ as str
    {
      Buffer.add_string buf str;
      read_string buf start_p lexbuf
    }
  | '\n' { Buffer.add_char buf '\n'; incr_line lexbuf; read_string buf start_p lexbuf }
  | _ { raise (LexerError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (LexerError ("String is not terminated")) }

{ }

