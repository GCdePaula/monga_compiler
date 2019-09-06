open Src


let rec parse lexbuf =
  let token = Lexer.monga_lexer lexbuf in
  let c_num = lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1 in
  Printf.printf "%d:%d -- " lexbuf.lex_curr_p.pos_lnum c_num;
  Types.string_of_tk token;
  Printf.printf "\n";

  match token with
    | Types.Eof -> ()
    | _ -> parse lexbuf


let test fileName =
  let cin = open_in fileName in
  let lexbuf = Lexing.from_channel cin in
  parse lexbuf


let main () =
  let fileName = "inputs/test1.in" in
  LexerTest.test fileName;
  let fileName = "inputs/test2.in" in
  LexerTest.test fileName


let _ = Printexc.print main ()

