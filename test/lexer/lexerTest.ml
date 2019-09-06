open Src
open Printf

let rec parse lexbuf =
  try
    let token = Lexer.monga_lexer lexbuf in
    let c_num = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1 in
    printf "%d:%d -- " lexbuf.lex_curr_p.pos_lnum c_num;
    Types.print_tk token;
    printf "\n";

    match token with
      | Types.Eof -> ()
      | _ -> parse lexbuf
  with
    Lexer.LexerError e -> printf "LexerError: %s\n" e;;

let test fileName =
  printf "LEXING FILE %s\n\n" fileName;
  let cin = open_in fileName in
  let lexbuf = Lexing.from_channel cin in
  parse lexbuf;
  printf "\nDONE! %s\n\n" fileName;;

let main () =
  let d = "inputs/" in
  let fnames = [d^"test1.in"; d^"test2.in"; d^"test3.in"] in
  List.map test fnames;;

let _ = Printexc.print main ()

