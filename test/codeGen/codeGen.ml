open Src
open Printf
open Llvm

let parse lexbuf =
  try
    let untyped_ast = Parser.program Lexer.monga_lexer lexbuf in

    match TypeCheck.build_typed_tree untyped_ast with
    | Ok typed_ast ->
      Utils.print_typed_program typed_ast
    | Error err_list ->
      Utils.print_type_error_list err_list
  with
    Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        printf "Syntax Error at %d:%d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

let test fileName =
  printf "PARSING FILE %s\n" fileName;

  let cin = open_in fileName in
  let lexbuf = Lexing.from_channel cin in
  let _ = parse lexbuf in

  printf "DONE %s\n\n" fileName

let main () =
  let d = "inputs/" in
  let fnames = [
    d^"test1.in";
    d^"test2.in";
    d^"test3.in";
    d^"test4.in";
  ] in

  List.map test fnames

let _ = Printexc.print main ()
