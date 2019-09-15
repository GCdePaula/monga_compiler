open Src
open Printf

let rec parse lexbuf =
  try
    let token = Lexer.monga_lexer lexbuf in
    let start_num = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1 in
    let cur_num = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
    printf "%d:(%d-%d) -- " lexbuf.lex_curr_p.pos_lnum start_num cur_num;
    Utils.print_tk token;
    printf "\n";

    match token with
      | Parser.EOF -> ()
      | _ -> parse lexbuf
  with
    Lexer.LexerError e -> printf "LexerError: %s\n" e

let test fileName =
  printf "LEXING FILE %s\n" fileName;

  let cin = open_in fileName in
  let lexbuf = Lexing.from_channel cin in
  parse lexbuf;

  printf "DONE %s\n\n" fileName

let main () =
  let d = "inputs/" in
  let fnames = [
    d^"test1.in";
    d^"test2.in";
    d^"test3.in";
    d^"test4.in";
    d^"test5.in";
    d^"test6.in";
    d^"test7.in";
    d^"test8.in";
    d^"test9.in";
    d^"test10.in";
    d^"test11.in";
  ] in

  List.map test fnames

let _ = Printexc.print main ()

