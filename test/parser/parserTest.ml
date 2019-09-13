open Src
open Printf

let parse lexbuf =
  try
      let _ = Parser.program Lexer.monga_lexer lexbuf in ();
  with
    Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        printf "Syntax Error at %d:%d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
    ;;


let test fileName =
  printf "PARSING FILE %s\n" fileName;

  let cin = open_in fileName in
  let lexbuf = Lexing.from_channel cin in
  let _ = parse lexbuf in

  printf "DONE %s\n\n" fileName;;

let main () =
  let d = "inputs/" in
  let fnames = [
    d^"test1.in";
  ] in

  List.map test fnames;;

let _ = Printexc.print main ()
