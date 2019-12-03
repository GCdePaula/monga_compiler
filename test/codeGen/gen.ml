open Src
open Llvm
open Base
open Stdio

exception TypeError of TypedAst.error list
exception ParserError of Lexing.position

let build_typed_tree lexbuf =
  try
    let untyped_ast = Parser.program Lexer.monga_lexer lexbuf in
    match TypeCheck.build_typed_tree untyped_ast with
    | Ok typed_ast ->
      typed_ast
    | Error err_list ->
      raise (TypeError err_list)
  with Parser.Error -> raise (ParserError lexbuf.lex_curr_p)

let test file_name =
  printf "FILE %s LLVM CODE\n" file_name;

  try
    let cin = In_channel.create file_name in
    let lexbuf = Lexing.from_channel cin in
    let typed_tree = build_typed_tree lexbuf in
    let llm = CodeGen.gen_code typed_tree in
    printf "%s\n" (string_of_llmodule llm);

    (
      match Llvm_analysis.verify_module llm with
      | None ->
        let open Ctypes in
        let open Foreign in

        let _ = Llvm_executionengine.initialize () in
        let exe_engine = Llvm_executionengine.create llm in
        let c_t = funptr (void @-> returning int64_t) in
        let main_func = Llvm_executionengine.get_function_address "main" c_t exe_engine in


        let monga_ret = main_func () in

        printf "RETURN %Ld\n\n" monga_ret;
        printf "OUTPUT\n";

        let _ = Llvm_executionengine.remove_module llm exe_engine in
        Llvm_executionengine.dispose exe_engine

      | Some reason ->
        printf "BAD LLVM CODE. REASON:\n%s\n" reason;
    )
  with
  | ParserError pos ->
    printf "Syntax Error at %d:%d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

  | TypeError err_list ->
    Utils.print_type_error_list err_list

